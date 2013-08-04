module MaffStrings where

import I18n
import MaffRoles

data MaffTheme = Mafia | Anonymous | Werewolf

msSunset German Mafia = "Es wird Nacht. Die Bürger gehen schlafen und die Mafia erwacht."
msSunset English Mafia = "Night begins. The citizens go to sleep and the Mafia awakes."
msSunset German Anonymous = "Es wird Nacht. Während alle Skiddies schlafen müssen, schieben die Feds fleißig Überstunden."
msSunset English Anonymous = "Night begins. While all the skiddies have to sleep, the Feds are working harder than ever before."
msSunrise German Mafia = "Die Sonne geht auf. Die Mafia hat ihre Arbeit erledigt und die Bürger wachen wieder auf."
msSunrise English Mafia = "The sun rises. The Mafiosi have done their work and the citizens meet in the town centre."
msSunrise German Anonymous = "Die Sonne geht auf. Die Skiddies treffen sich im Angriffschannel und beraten sich über das nächste LOIC-Ziel."
msSunrise English Anonymous = "The sun rises. The skiddies are meeting in the attack channel and consulting about the next LOIC target."

msKilled German Mafia = "%s ist in der Nacht gestorben."
msKilled English Mafia = "%s has died in the night."
msKilled German Anonymous = "%s wurde gev&."
msKilled English Anonymous = "%s got v&."

msNooneKilled German Mafia = "In dieser Nacht ist niemand gestorben."
msNooneKilled English Mafia = "This night noone has died."
msNooneKilled German Anonymous = "Dieses Mal wurde niemand gev&."
msNooneKilled English Anonymous = "This time noone has been v&."

msDimitriKilled German Mafia = "In dieser Nacht wurde %s vom Rasenmäher überfahren."
msDimitriKilled English Mafia = "This night %s was run over by a lawn mower."
msDimitriKilled German Anonymous = undefined
msDimitriKilled English Anonymous = undefined

msExecuted German Mafia = "%s wurde gehängt."
msExecuted English Mafia = "%s has been executed."
msExecuted German Anonymous = "%s wurde weggel4z0rt."
msExecuted English Anonymous = "%s got l4z0red."

msCitizensWon German Mafia = "Die Bürger haben gewonnen! Juhu!"
msCitizensWon English Mafia = "The citizens have won! Woohoo!"
msCitizensWon German Anonymous = "Anonymous hat gewonnen! Nyan! Nyan! Nyan! Nyan!"
msCitizensWon English Anonymous = "Anonymous has won! Nyan! Nyan! Nyan! Nyan!"

msMafiaWon German Mafia = "Die Mafia hat gewonnen. Verbrecher beherrschen die Erde."
msMafiaWon English Mafia = "The Mafia has won. Criminals are ruling the world."
msMafiaWon German Anonymous = "Oh nein! Die Feds haben gewonnen :("
msMafiaWon English Anonymous = "Oh noes! The Feds won :("

showSuit German _ Club = "Kreuz"
showSuit German _ Spade = "Pik"
showSuit German _ Heart = "Herz"
showSuit German _ Diamond = "Karo"
showSuit English _ s = show s

showValue German _ Seven = "Sieben"
showValue German _ Eight = "Acht"
showValue German _ Nine = "Neun"
showValue German _ Ten = "Zehn"
showValue German _ Jack = "Bube"
showValue German _ Queen = "Dame"
showValue German _ King = "König"
showValue German _ Ace = "Ass"
showValue English _ v = show v

showCard German (s,v) = show s ++ " " ++ show v
showCard English (s,v) = show v ++ " of " ++ show s ++ "s"

-- Rollenloses Opfer
msExplainRole German Mafia Rop =
  "Du bist unschuldiger Bürger. Du musst mit den anderen Bürgern versuchen, die Mafiosi" ++
  " zu besiegen, indem ihr sie am Tage hängt."
msExplainRole English Mafia Rop =
  "You are an innocent citizen. Together with the other citizens you have to superfice the" ++
  " Mafiosi by executing them by daylight."
msExplainRole German Anonymous Rop =
  "Du bist ahnungsloses Skriptkiddie. Zusammen mit deinen Artgenossen solltest du es den Feds" ++
  " so richtig zeigen, indem du sie bei Tage in Grund und Boden l4z0rst!"
msExplainRole English Anonymous Rop =
  "You are a dumb script kiddie. Together with your kind you should teach the Feds a lesson" ++
  " by l4z0ring them to dust!"
  
-- Rollenloser Mafioso
msExplainRole German Mafia Rom =
  "Du bist Mafioso. Dein Ziel ist es, alle Bürger auszulöschen, indem du sie zusammen" ++
  " mit deiner Mafia bei Nacht umlegst. Du kannst dich mit deinen Komplizen über den Befehl" ++
  " '!mafftalk' unterhalten, damit ihr euch gegenseitig erkennt und absprechen könnt."
msExplainRole English Mafia Rom =
  "You are a Mafioso. It is your aim to exstinguish all citizens by murdering them by night." ++
  " Your accomplices will help you. You can talk with them using the command '!mafftalk'." ++
  " That way you can find each other and plan your next actions."
msExplainRole German Anonymous Rom =
  "Du bist ein Fed und somit Diener des Staates. Dein Ziel ist es, den bösen H4xx0rz von" ++
  " Anonymous zu zeigen, was eine Harke ist. Spüre sie auf und führe sie ihrer gerechten" ++
  " Strafe zu! Dazu kannst du dich mit den anderen Feds über den Befehl '!mafftalk' unterhalten," ++
  " damit ihr euch gegenseitig findet und absprechen könnt."
msExplainRole English Anonymous Rom =
  "You are a fed and as such you serve the state. Your aim is to teach the evil h4xx0rs" ++
  " a lesson they never forget. Find them and let them feel justice! You can talk with your" ++
  " fellows using the command '!mafftalk'. That way you can find each other and plan your" ++
  " next actions."
  
-- Mafia-Spion (kann die Rolle einer Person überprüfen)
msExplainRole German Mafia Spy =
  "Du bist Spion. Als Spion gehörst du zur Mafia, aber du hast eine Spezialfähigkeit: Du" ++
  " kannst nachts die Rolle einer anderen Person überprüfen."
msExplainRole English Mafia Spy =
  "You are a spy. As a spy you belong to the Mafia, but you've got a special ability: You" ++
  " can check someone else's role by night."
msExplainRole German Anonymous Spy = undefined
msExplainRole English Anonymous Spy = undefined
  
-- Justin (hat gewonnen, wenn er am ersten Tag gehängt wurde)
msExplainRole German Mafia Justin =
  "Du bist Justin. Als Justin ist es dein Ziel, am ersten Tag gehängt zu werden. Wenn du das" ++
  " wirst, endet das Spiel sofort und du hast gewonnen. Andernfalls kannst du immer noch" ++
  " mit den anderen Bürgern gewinnen. Viel Glück!"
msExplainRole English Mafia Justin = 
  "You are Justin. As Justin your aim is to be executed on the first day. If you are, the game" ++
  " is over and you have won. Otherwise, you can still win with the other citizens. Good luck!"
msExplainRole German Anonymous Justin = undefined
msExplainRole English Anonymous Justin = undefined
  
-- Dimitri (stirbt in der ersten Nacht, wenn kein anderer stirbt)
msExplainRole German Mafia Dimitri = 
  "Du bist Dimitri. Als Dimitri bist du unschuldiger Bürger, aber ziemlich dumm. Daher wirst" ++
  " du in der ersten Nacht vom Rasenmäher überfahren, wenn niemand anders stirbt. Viel Glück!"
msExplainRole English Mafia Dimitri =
  "You are Dimitri. As Dimitri you are an innocent citizen, but very stupid. Thus, if nobody" ++
  " else dies in the first night, you are run over by a lawn mower. Good luck!"
msExplainRole German Anonymous Dimitri = undefined
msExplainRole English Anonymous Dimitri = undefined
  
-- Detektiv (kann die Fraktion eines Spielers überprüfen)
msExplainRole German Mafia Detective =
  "Du bist Detektiv. Als Detektiv bist du Bürger, aber du hast jede Nacht die Möglichkeit," ++
  " die Fraktion eines Mitspielers zu überprüfen."
msExplainRole English Mafia Detective = 
  "You are detective. As the detective you are a citizen, but every night you have the chance" ++
  " to check someone else's fraction."
msExplainRole German Anonymous Detective = undefined
msExplainRole English Anonymous Detective = undefined

-- Inspektor (kann die Rolle eines Spielers überprüfen)
msExplainRole German Mafia Inspector =
  "Du bist Inspektor. Als Inspektor bist du Bürger, aber du hast jede Nacht die Möglichkeit," ++
  " die Rolle eines Mitspielers zu überprüfen."
msExplainRole English Mafia Inspector =
  "You are inspector. As the inspector you are a citizen, but every night you have the chance" ++
  " to check someone else's role."
msExplainRole German Anonymous Inspector = undefined
msExplainRole English Anonymous Inspector = undefined
  
-- Nullteiler (Bürger, kann durch Null teilen und damit sich und einen anderen Spieler in die Luft jagen)
msExplainRole German Mafia ZeroDivisor =
  "Du bist Nullteiler. Du bist Bürger, aber wann immer du möchtest, kannst du eine andere" ++
  " Person durch Null teilen und damit töten. Der Nachteil: Du wirst dabei selbst auch sterben."
msExplainRole English Mafia ZeroDivisor =
  "You are a zero-divisor. You are a citizen, but whenever you want, you can divide" ++
  " a person by zero and kill him/her. The disadvantage: You will die during that process, too."
msExplainRole German Anonymous ZeroDivisor = undefined
msExplainRole English Anonymous ZeroDivisor = undefined
  
-- Gärtner (Bürger, wird aber vom Dete als Maff angezeigt)
msExplainRole German Mafia Gardener =
  "Du bist Gärtner. Du bist Bürger, aber für den Detektiv siehst du recht verdächtig aus..."
msExplainRole English Mafia Gardener =
  "You are gardener. You are a citizen, but to the detective you look quite suspicious ..."
msExplainRole German Anonymous Gardener = undefined
msExplainRole English Anonymous Gardener = undefined
  
-- Terrorist (Mafioso, der aber in der Nacht nicht aufwacht. Kann sich am Tag mit einem anderen Spieler in die Luft jagen, solange er noch nicht angeklagt ist)
msExplainRole German Mafia Terrorist =
  "Du bist Terrorist. Du gewinnst, wenn die Mafia siegt, aber du kennst die Mafia nicht und sie" ++
  " kennen dich nicht. Du kannst nachts ein T-Zeichen zeigen (!tsign), um ihnen einen Hinweis zu" ++
  " geben, aber das kann jeder andere auch. Dein Vorteil ist, dass du tagsüber explodieren kannst," ++
  " wenn du willst, und damit tötest du eine weitere Person mit dir, aber das geht nur solange du" ++
  " nicht zur Anklage aufgestellt bist."
msExplainRole English Mafia Terrorist =
  "You are terrorist. You win when the Mafia does, but you don't know the Mafiosi and they don't" ++
  " know you. You can show a T-sign by night (!tsign) to give them a hint, but everyone else" ++
  " could do that as well. Your advantage is that you can explode by daylight when you choose so," ++
  " and you kill another person with that, too, but only as long as you are not proposed for" ++
  " execution."
msExplainRole German Anonymous Terrorist = undefined
msExplainRole English Anonymous Terrorist = undefined
  
-- Busfahrer (kann in der Nacht zwei Personen auswählen; passiert irgendetwas (Mord, Seelenrettung, etc.) mit Person A, geschieht es in Wirklichkeit Person B; und andersherum)
msExplainRole German Mafia BusDriver =
  "Du bist Busfahrer. Du bist Bürger, aber jede Nacht kannst du zwei Personen auswählen und ihr" ++
  " Schicksal vertauschen. Wenn eine Person von der Mafia getötet wird, stirbt die andere. Das" ++
  " gleiche gilt für Seelenrettung, Amors Pfeil und die Recherchen von Detektiv, Inspektor und" ++
  " allen Spionen. Wähle deine Ziele gut, diese Rolle kann sehr mächtig sein, aber auch sehr" ++
  " nervig für deine Mitbürger."
msExplainRole English Mafia BusDriver =
  "You are bus driver. You are citizen, but by night you can choose two persons and swap their" ++
  " destiny. If one person is killed by the Mafia, the other one dies. Same for soul protection," ++
  " Cupid's strike, and the checks of the detective, inspector and any spies. Choose your targets" ++
  " well, this role can be very mighty, but also very crappy."
msExplainRole German Anonymous BusDriver = undefined
msExplainRole English Anonymous BusDriver = undefined
  
-- Seelenretter (kann in der Nacht eine Person auswählen, die auch dann nicht stirbt, wenn sie Opfer eines Mafiakills w rde)
msExplainRole German Mafia SoulSaver =
  "Du bist Seelenretter. Du bist Bürger, aber nachts kannst du eine Person schützen, sodass diese" ++
  " nicht durch die Hand der Mafia stirbt. Wähle dein Ziel gut, du kannst eine Person nicht" ++
  " mehrmals schützen!"
msExplainRole English Mafia SoulSaver =
  "You are soul saver. You are citizen, but by night you can shield a person, such that he/she" ++
  " doesn't die by the Mafia's sword. Choose your target well, you can't protect one person twice!"
msExplainRole German Anonymous SoulSaver = undefined
msExplainRole English Anonymous SoulSaver = undefined
  
-- Jäger (wenn er am Tage stirbt, kann er eine Person mit in den Tod rei en)
msExplainRole German Mafia Hunter =
  "Du bist Jäger. Du bist Bürger, aber wenn du am Tage stirbst, kannst du immer noch eine" ++
  " andere Person sofort mit in den Tod reißen."
msExplainRole English Mafia Hunter =
  "You are hunter. You are a citizen, but if you die by daylight, you can still kill one" ++
  " other person immediately after your execution."
msExplainRole German Anonymous Hunter = undefined
msExplainRole English Anonymous Hunter = undefined
  
-- Papst (kann jede Nacht eine Person auswählen, die er zum Mönch macht; alle Mönche kennen sich gegenseitig; versucht der Papst, einen Mafioso zu bekehren, stirbt er)
msExplainRole German Mafia Pope =
  "Du bist der Papst. Du bist Bürger, aber nachts kannst du versuchen, eine Person zu bekehren." ++
  " Ist die gewählte Person Bürger, wird diese zum Mönch, andernfalls wirst du sterben." ++
  " Alle Mönche kennen sich gegenseitig."
msExplainRole English Mafia Pope =
  "You are the Pope. You are citizen, but by night you can try to convert a person." ++
  " If the selected person is a citizen, he/she will become a monk, otherwise you will die." ++
  " All monks know each other."
msExplainRole German Anonymous Pope = undefined
msExplainRole English Anonymous Pope = undefined
  
-- Amor (kann in der ersten Nacht zwei Personen verkuppeln. Stirbt der eine, bringt sich der andere selbst um)
msExplainRole German Mafia Amor =
  "Du bist Amor. Du bist Bürger, aber in der ersten Nacht kannst du zwei Personen auswählen," ++
  " die ein Liebespaar sein werden. Stirbt eine davon, begeht die andere Selbstmord. Ist eine" ++
  " von ihnen Mafioso und die andere Bürger, bilden sie eine neue Fraktion und können nur" ++
  " gewinnen, wenn sie am Ende die letzten beiden lebenden Seelen sind."
msExplainRole English Mafia Amor =
  "You are Cupid. You are a citizen, but in the first night you can choose two persons, who" ++
  " will form a couple. If one of them dies, the other one commits suicide. If one of them" ++
  " is a mafioso and the other one is a citizen, they form a new fraction and can only win if" ++
  " they are the last two living souls in the end."
msExplainRole German Anonymous Amor = undefined
msExplainRole English Anonymous Amor = undefined

-- Stoiber (kann jede Nacht eine Person auswählen, die am folgenden Tag weder reden noch abstimmen darf)
msExplainRole German Mafia Stoiber =
  "Du bist Stoiber. Du bist Bürger, aber jede Nacht kannst du eine Person auswählen, die am" ++
  " nächsten Tag weder sprechen noch abstimmen kann. Die meiste Zeit über wird dir dies nicht" ++
  " helfen, aber hin und wieder... Sei aufmerksam und lass den richtigen Moment nicht ungenutzt" ++
  " vergehen!"
msExplainRole English Mafia Stoiber =
  "You are Stoiber. You are citizen, but every night you can choose a person, which can neither" ++
  " speak nor vote on the next day. Most of the time, this won't help you, but sometimes..." ++
  " Be attentive and don't let the right moment pass unused!"
msExplainRole German Anonymous Stoiber =
  "Du bist Zensursula. Durch eine geistige Umnachtung gehörst du zu den Anons, obwohl du es" ++
  " eigentlich gar nicht willst. Jede Nacht kannst du eine Person auswählen, die am nächsten" ++
  " Tag weder sprechen noch abstimmen kann. Die meiste Zeit über wird dir dies nicht" ++
  " helfen, aber hin und wieder... Sei aufmerksam und lass den richtigen Moment nicht ungenutzt" ++
  " vergehen!"
msExplainRole English Anonymous Stoiber = undefined

msRoleName German       Mafia           Rop             = "Unschuldiger Bürger"
msRoleName English      Mafia           Rop             = "Innocent Citizen"
msRoleName German       Anonymous       Rop             = "Ahnungsloses Skriptkiddie"
msRoleName English      Anonymous       Rop             = "Stupid Script Kiddie"
msRoleName German       Mafia           Rom             = "Gemeiner Mafioso"
msRoleName English      Mafia           Rom             = "Evil Mafioso"
msRoleName German       Anonymous       Rom             = "08/15 Fed"
msRoleName English      Anonymous       Rom             = "08/15 Fed"
msRoleName German       Mafia           Spy             = "Mafia-Spion"
msRoleName English      Mafia           Spy             = "Mafia Spy"
msRoleName German       Anonymous       Spy             = "Cleverer Fed"
msRoleName English      Anonymous       Spy             = "Clever Fed"
msRoleName German       Mafia           Justin          = "Justin"
msRoleName English      Mafia           Justin          = "Justin"
msRoleName German       Anonymous       Justin          = "Wopot"
msRoleName English      Anonymous       Justin          = "Wopot"
msRoleName German       Mafia           Dimitri         = "Dimitri"
msRoleName English      Mafia           Dimitri         = "Dimitri"
msRoleName German       Anonymous       Dimitri         = "Michael Renner"
msRoleName English      Anonymous       Dimitri         = "Michael Renner"
msRoleName German       Mafia           Detective       = "Detektiv"
msRoleName English      Mafia           Detective       = "Detective"
msRoleName German       Anonymous       Detective       = "Journalist"
msRoleName English      Anonymous       Detective       = "Journalist"
msRoleName German       Mafia           Inspector       = "Inspektor"
msRoleName English      Mafia           Inspector       = "Inspector"
msRoleName German       Anonymous       Inspector       = "Investigativer Journalist"
msRoleName English      Anonymous       Inspector       = "Investigative Journalist"
msRoleName German       Mafia           ZeroDivisor     = "Nullteiler"
msRoleName English      Mafia           ZeroDivisor     = "Zero-Divisor"
msRoleName German       Anonymous       ZeroDivisor     = "Blackhat"
msRoleName English      Anonymous       ZeroDivisor     = "Blackhat"
msRoleName German       Mafia           Terrorist       = "Terrorist"
msRoleName English      Mafia           Terrorist       = "Terrorist"
msRoleName German       Anonymous       Terrorist       = "Whitehat"
msRoleName English      Anonymous       Terrorist       = "Whitehat"
msRoleName German       Mafia           SoulSaver       = "Seelenretter"
msRoleName English      Mafia           SoulSaver       = "Soul Saver"
msRoleName German       Anonymous       SoulSaver       = "Ecuadorianischer Botschafter"
msRoleName English      Anonymous       SoulSaver       = "Ecuadorian Ambassador"
msRoleName German       Mafia           BusDriver       = "Busfahrer"
msRoleName English      Mafia           BusDriver       = "Bus Driver"
msRoleName German       Anonymous       BusDriver       = "Bekiffter Telekom-SysAdmin"
msRoleName English      Anonymous       BusDriver       = "Stoned TelCo SysAdmin"
msRoleName German       Mafia           Stoiber         = "Stoiber"
msRoleName English      Mafia           Stoiber         = "Stoiber"
msRoleName German       Anonymous       Stoiber         = "Zensursula von der Leyen"
msRoleName English      Anonymous       Stoiber         = "Censorsula von der Leyen"
msRoleName German       Mafia           Pope            = "Papst"
msRoleName English      Mafia           Pope            = "Pope"
msRoleName German       Anonymous       Pope            = "Johannes Ponader"
msRoleName English      Anonymous       Pope            = "Johannes Ponader"
msRoleName German       Mafia           Hunter          = "Jäger"
msRoleName English      Mafia           Hunter          = "Hunter"
msRoleName German       Anonymous       Hunter          = "Julian Assange"
msRoleName English      Anonymous       Hunter          = "Julian Assange"
msRoleName German       Mafia           Gardener        = "Gärtner"
msRoleName English      Mafia           Gardener        = "Gärtner"
msRoleName German       Anonymous       Gardener        = "j4rkill"
msRoleName English      Anonymous       Gardener        = "j4rkill"
msRoleName German       Mafia           Tree            = "Baum"
msRoleName English      Mafia           Tree            = "Tree"
msRoleName German       Anonymous       Tree            = "Kim Schmitz"
msRoleName English      Anonymous       Tree            = "Kim Schmitz"
