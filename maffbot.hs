import Data.List
import Network
import System.IO
import System.Exit
import System.Random
import Control.Arrow
import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import Text.Printf
import Prelude hiding (catch)
import MaffParty
import I18n
import qualified MaffStrings as MS

{-server = "irc.anonymous-austria.com"
port   = 6667
chan   = "#mafiapardey"
nick   = "James"
passwd = "testjames"-}
server = "localhost"
port = 6677
chan = "#"
nick = "James"
passwd = ""

-- The 'Net' monad: Wrapping IO, carrying the mutable state in the StateT transformer
-- and the immutable one in the ReaderT transformer
type Net = StateT Mutable (ReaderT Bot IO)
data Bot = Bot { socket :: Handle }
data Mutable = Mutable { party :: Party, users :: [String] } deriving Eq

data Party = ResistanceParty ResistanceParty | MafiaParty MafiaParty | TimeOut deriving Eq
data ResistanceParty = Resistance () () deriving (Show,Eq)

instance Show Party where
  show TimeOut = "No game running"
  show (MafiaParty mp) = show mp
  show (ResistanceParty rp) = show rp

instance Show Mutable where
  show (Mutable p u) = ([show p, "; Users: "]++intersperse ", " u) >>= id

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect (\x -> do l <- loop x; return ())
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT (runStateT run (Mutable TimeOut [])) st) (\(SomeException _) -> return ((),Mutable TimeOut []))

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :James")
    write "JOIN" chan
    asks socket >>= listen

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init <$> io (hGetLine h)
    io (putStrLn $ concat ["IN  ",s])
    m1 <- get
    if ping s then pong s
              else eval (extractSender s) (extractContext s) s
    m2 <- get
    if m1 /= m2 then io (putStrLn $ concat ["STA ",show m2]) else return ()
  where
    forever a = a >> forever a
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- Dispatch a command
eval :: String -> String -> String -> Net ()
eval sender "PRIVMSG" msg = evalPrivmsg sender (clean msg) (extractTarget msg)
eval sender "NOTICE" msg = evalNotice sender (clean msg) (extractTarget msg)
eval _ "404" _ = write "JOIN" chan
eval _ "353" msg = do p <- gets party; put (Mutable p (evalNickList msg)); syncModes
eval sender "PART" _ = do
  (Mutable p u) <- get
  put (Mutable p (delete (nickFromMask sender) u))
  u' <- gets users
  case p of
       (MafiaParty (PendingMafia ps)) -> put (Mutable (MafiaParty (PendingMafia (delete (nickFromMask sender) ps))) u')
       px -> return ()
eval sender "QUIT" t = eval sender "PART" t
eval sender "JOIN" _ = do 
  (Mutable p u) <- get
  put (Mutable p (nickFromMask sender :u))
  syncModes
  if nick /= nickFromMask sender then privmsg chan $ concat ["Welcome, ",nickFromMask sender,"!"]
                    else return ()
  case p of
       (MafiaParty (PendingMafia ps)) -> privmsg chan $ concat [nickFromMask sender,", do you want to participate in the pending Mafia party? Just type '!join'!"]
       px -> return ()
eval _ _ _                = return () -- ignore everything else

-- Eval PRIVMSG
evalPrivmsg :: String -> String -> String -> Net ()
evalPrivmsg _ "!quit" _                 = do
  u <- gets users
  put (Mutable TimeOut u)
  syncModes
  write "QUIT" ":James got a lot to do!"
  io (exitWith ExitSuccess)
evalPrivmsg s x t | "!id " `isPrefixOf` x = reply s (drop 4 x) t
                  | "!status" == x = do
                      m <- gets show
                      reply s m t
                  | "!mafia" == x = do 
                      u <- gets users
                      put (Mutable (MafiaParty (PendingMafia [])) u)
                      privmsg chan "A new Mafia party has been triggered. If you want to participate, type '!join'. Game starts on '!start'."
                  | "!resistance" == x = do
                      u <- gets users
                      put (Mutable (ResistanceParty (Resistance () ())) u)
                  | "!cancel" == x = do
                      u <- gets users
                      privmsg chan $ concat [nickFromMask s," has cancelled the party."]
                      put (Mutable TimeOut u)
                      syncModes
                  | "!start" == x = do
                      (Mutable p u) <- get
                      r <- io (getStdRandom random)
                      let p' = case p of
                           (MafiaParty p@(PendingMafia _)) -> (MafiaParty (startMafiaParty p r))
                           px -> px
                      case p' of
                           (MafiaParty (NoMafiaCuz err)) -> privmsg chan err
                           px -> do
                             put (Mutable p' u)
                             if p == p' then privmsg chan "You have to announce a party first. Use !mafia or !resistance."
                                        else do
                                          privmsg chan "The party has been started."
                                          case p' of
                                               (MafiaParty (Mafia ps _ _)) -> initFirstNight
                                               px -> return ()
                  | "!join" == x = do
                      (Mutable p u) <- get
                      let p' = case p of
                           (MafiaParty (PendingMafia ps)) -> (MafiaParty (PendingMafia (union ps [nickFromMask s])))
                           px -> px
                      put (Mutable p' u)
                      if p == p' then privmsg chan "Either you have already joined or there is no party announced."
                                 else privmsg chan "You have joined the announced party."
                  | "!mafftalk" `isPrefixOf` x =
                      maffOnly s isMafioso always (\mp me ->
                        forM_ (filter isMafioso $ participants mp) (\pa -> privmsg (partNick pa) $
                        concat [nickFromMask s," whispered: ",drop 10 x]))
                  | "!tsign" == x = do
                      maffOnly s always isNight (\mp me ->
                        forM_ (filter isMafioso $ participants mp) (\pa -> privmsg (partNick pa) $
                        concat [nickFromMask s," shows a T-sign. Maybe he's the terrorist?"]))
                  | "!" `isPrefixOf`x = reply s "I'm sorry, didn't quite catch that." t
evalPrivmsg _ _ _                       = return () -- ignore everything else

-- Eval NOTICE
evalNotice :: String -> String -> String -> Net ()
evalNotice s x "James" | isPrefixOf "NickServ" s && isPrefixOf "This nickname" x = privmsg "NickServ" $ concat ["identify ",passwd]
                       | isPrefixOf "HostServ" s && isPrefixOf "Your vhost" x = write "JOIN" chan
evalNotice _ _ _ = return ()

-- Send a privmsg to the current chan + server
privmsg :: String -> String -> Net ()
privmsg ch s = write "PRIVMSG" (ch ++ " :" ++ s)

-- Reply to a PRIVMSG
reply :: String -> String -> String -> Net ()
reply s msg t = if t==nick then privmsg (nickFromMask s) msg else privmsg chan msg

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "OUT %s %s\n" s t
    
syncModes :: Net ()
syncModes = do
  (Mutable p u) <- get
  let flagAll b c ns = write "MODE" $ concat [chan,if b then " +" else " -",take (length ns) $ repeat c," ",concat $ intersperse " " ns]
      voiceAll = flagAll True 'v'
      unvoiceAll = flagAll False 'v'
      ensureModeration = write "MODE" $ concat [chan," +m"]
  case p of
       (MafiaParty (Mafia ps _ Day)) -> do
         voiceAll $ fmap partNick ps
         unvoiceAll $ filter (not.(`elem` (fmap partNick ps))) u
         ensureModeration
       (MafiaParty (Mafia _ _ _)) -> do
         unvoiceAll u
         ensureModeration
       px -> do
         voiceAll u
         flagAll False 'o' u
         flagAll False 'h' u
         ensureModeration

-- Extract nick from nick!user@host masks
nickFromMask :: String -> String
nickFromMask = takeWhile (/= '!')

-- Extract message text from PRIVMSGs and NOTICEs
clean     = drop 1 . dropWhile (/= ':') . drop 1
-- Extract sender from incoming strings
extractSender = takeWhile (/= ' ') . drop 1
-- Extract command from incoming strings
extractContext = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
-- Extract target from PRIVMSGs and NOTICEs
extractTarget = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ') . drop 1 . dropWhile(/= ' ')

-- Evaluate nick list
evalNickList :: String -> [String]
evalNickList msg = delete nick $ 
                   fmap (dropWhile (`elem` ['@','&','%','+'])) $ 
                   words $
                   clean msg
                   
-- Only possible as a Mafioso/Citizen/etc.
maffOnly :: String -> (Participant -> Bool) -> (MafiaParty -> Bool) -> (MafiaParty -> Participant -> Net ()) -> Net ()
maffOnly s pf mf f = do
  let n = nickFromMask s
  p <- gets party
  case p of
       (MafiaParty mp@(Mafia _ _ _)) -> do
          let pa = participantFromNick n mp
          if pf pa  && mf mp then f mp pa
                             else return ()
       px -> return ()
       
-- Initialize first night
initFirstNight :: Net ()
initFirstNight = do
  (MafiaParty mp@(Mafia ps _ _)) <- gets party
  u <- gets users
  -- Give/take voices
  syncModes
  -- Announce the participants
  privmsg chan $ concat ("Let's see who's playing... There are ":intersperse ", " (fmap partNick ps))
  -- Instruct the players
  mapM_ (\pa -> privmsg (partNick pa) $ getMafiaIntroStr pa English MS.Mafia) ps
  -- Announce the night
  privmsg chan $ MS.msSunset English MS.Mafia
  -- Amor?
  if existsAmor mp then privmsg (partNick $ head $ filter isAmor $ participants mp) "Please select the couple using '!couple nick1 nick2'."
                   else initNight False
                
-- Initialize the night
initNight :: Bool -> Net()
initNight announce = do
  (MafiaParty (Mafia ps dn dt)) <- gets party
  u <- gets users
  return ()

-- Convenience.
io :: IO a -> Net a
io = liftIO

