module MaffRoles where

data CardSuit = Club | Spade | Heart | Diamond deriving (Eq,Show)
data CardValue = Two | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq,Show)
data Role = Rop | Rom | Spy | Justin | Dimitri | Detective | Inspector | ZeroDivisor |
            Gardener | Terrorist | BusDriver | SoulSaver | Hunter | Pope | Amor |
            Stoiber | Doctor | MaffJudge | CitJudge | Tree | NPSOwner | 
            OilTanker | Pyromanian | PlagiarismHunter | Guttenberg deriving (Eq,Show)
data Fraction = Citizen | Mafioso deriving (Eq,Show)

-- Fractions
fractionOf Rop          = Citizen
fractionOf Rom          = Mafioso
fractionOf Spy          = Mafioso
fractionOf Justin       = Citizen
fractionOf Dimitri      = Citizen
fractionOf Detective    = Citizen
fractionOf Inspector    = Citizen
fractionOf ZeroDivisor  = Citizen
fractionOf Gardener     = Citizen
fractionOf Terrorist    = Mafioso
fractionOf BusDriver    = Citizen
fractionOf SoulSaver    = Citizen
fractionOf Hunter       = Citizen
fractionOf Pope         = Citizen
fractionOf Amor         = Citizen

-- Roles
roleOf Spade    Ace     = Detective
roleOf Club     Ace     = Detective
roleOf Diamond  Ace     = Inspector
roleOf Heart    Ten     = Justin
roleOf Club     Ten     = Dimitri
roleOf Diamond  Ten     = Gardener
roleOf Spade    Ten     = ZeroDivisor
roleOf Club     King    = Terrorist
roleOf Spade    King    = Hunter
roleOf Diamond  King    = Stoiber
roleOf Heart    King    = Pope
roleOf Heart    Queen   = Amor
roleOf Spade    Queen   = SoulSaver
roleOf Diamond  Queen   = BusDriver
roleOf Club     Queen   = Doctor
roleOf Spade    Two     = Spy
roleOf Club     Two     = Rom
roleOf _        Seven   = Rop
roleOf _        Eight   = Rop
roleOf _        Nine    = Rop
roleOf _        Jack    = Rom
roleOf _        _       = Rop

-- Role Depot
mafiosi = (zip [Club,Spade,Heart,Diamond] $ repeat Jack) ++ [(Club,Two)]
citizens = zip (take 3 $ cycle [Club,Spade,Heart,Diamond]) $ cycle [Seven,Eight,Nine]
detectives = [(Spade,Ace),(Club,Ace)]
inspectors = [(Diamond,Ace)]
justins = [(Heart,Ten)]
dimitris = [(Club,Ten)]
terrorists = [(Club,King)]
gardeners = [(Diamond,Ten)]
hunters = [(Spade,King)]
amors = [(Heart,Queen)]
soulsavers = [(Spade,Queen)]
popes = [(Heart,King)]
zerodivisors = [(Spade,Ten)]
spies = [(Spade,Two)]
stoibers = [(Diamond,King)]
busdrivers = [(Diamond,Queen)]
