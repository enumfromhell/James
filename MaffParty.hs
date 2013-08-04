module MaffParty where

import Data.List
import MaffRoles
import I18n
import qualified MaffStrings as MS
import System.Random
import System.Random.Shuffle

data MafiaParty = PendingMafia [String] |
                  NoMafiaCuz String |
                  Mafia {
                    participants :: [Participant],
                    dayno :: Int,
                    daytime :: TimeOfDay
                  } deriving Eq

data Participant = Participant {
                      partNick :: String,
                      partCard :: (CardSuit,CardValue),
                      partDead :: Bool
                    } deriving Eq
                    
data TimeOfDay = EarlyFirstNight | Night | Day deriving (Show,Eq)

instance Show MafiaParty where
  show (PendingMafia ps) = concat ("Pending Mafia game with: ":intersperse ", " ps)
  show (Mafia ps dn dt) = concat ["Mafia game at ",show dt," ",show dn]

startMafiaParty :: MafiaParty -> Int -> MafiaParty
startMafiaParty (PendingMafia ps) r 
-- | length ps < 4 = NoMafiaCuz "We need at least four players."
  | otherwise =  let cs = shuffle' cs' (length ps) rg
                     (rx,rg) = random $ mkStdGen r
                     cs' = getMafiaCards (length ps) rx
                 in Mafia (zipWith3 Participant ps cs $ repeat False) 0 EarlyFirstNight
startMafiaParty m  _ = m

getMafiaCards :: Int -> Int -> [(CardSuit,CardValue)]
getMafiaCards len r
  | len < 4 = take len citizens
  | len < 7 = take 2 mafiosi ++ take 1 detectives ++ take (len-3) citizens
  | len < 11 = take 2 mafiosi ++ take 1 detectives ++ (take 1 . arb rg1) ssroles ++
               (take (len-4) . arb rg2) optroles1
  | otherwise = take 2 mafiosi ++ take 1 detectives ++ (take 1 . arb rg1) ssroles ++
               (take (len-5) . arb rg2) optroles2 ++ (take 1 $ drop 4 mafiosi)
  where (rx,rg1) = random $ mkStdGen r :: (Int,StdGen)
        rg2 = mkStdGen rx
        arb rg cs = shuffle' cs (length cs) rg
        ssroles = concat [ take 1 soulsavers, take 1 busdrivers ]
        optroles1 = concat [
            take 1 detectives,
            take 1 inspectors,
            take 1 hunters,
            take 1 terrorists,
            take 1 justins,
            take 1 dimitris,
            take 1 gardeners,
            take 3 citizens,
            take 1 amors,
            take 2 $ drop 2 mafiosi
          ]
        optroles2 = concat [
            optroles1,
            take 1 popes,
            take 1 zerodivisors,
            take 1 spies,
            take 3 $ drop 3 citizens,
            take 1 stoibers
          ]
  
getMafiaIntroStr :: Participant -> Language -> MS.MaffTheme -> String
getMafiaIntroStr (Participant n c@(cs,cv) _) l t = concat ["You got a ",MS.showCard l c,"! ",MS.msExplainRole l t (roleOf cs cv)]

isFraction :: Fraction -> Participant -> Bool
isFraction fract (Participant _ (cs,cv) _) = fractionOf (roleOf cs cv) == fract
isMafioso = isFraction Mafioso
isCitizen = isFraction Citizen
isRole :: Role -> Participant -> Bool
isRole role (Participant _ (cs,cv) _) = roleOf cs cv == role
isDetective = isRole Detective
isSoulSaver = isRole SoulSaver
isAmor = isRole Amor

isTimeOfDay :: TimeOfDay -> MafiaParty -> Bool
isTimeOfDay tod (Mafia _ _ t) = t == tod
isNight = not . isTimeOfDay Day
isEarlyFirstNight = isTimeOfDay EarlyFirstNight
isDay = isTimeOfDay Day
existsRole :: Role -> MafiaParty -> Bool
existsRole r (Mafia ps _ _) = any (isRole r) ps
existsAmor = existsRole Amor

always = const True
never = const False

participantFromNick :: String -> MafiaParty -> Participant
participantFromNick n (Mafia ps _ _) = head $ filter ((==n).partNick) ps
