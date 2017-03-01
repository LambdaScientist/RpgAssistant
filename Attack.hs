{-# LANGUAGE RecordWildCards #-}

import Prelude
import Data.Monoid

main = undefined 
--TODO: Add Pretty print
--TODO: Add monoid for these types to prevent duplicates from appearing...Maybe?
--TODO: Add empty meta type
--TODO: Add Player calc
--TODO: Finish Attack Calc
data ResourceDrain = SpellLevelIncrease Int | ManaRequirement Int deriving (Show) 
data DamageType = Piercing | Bludgeoning | Slashing | Fire | Acid | Cold | Lightning | NoType deriving (Show) 
data AbilityType = Strength | Dexterity | Intelligence | Constitution | Wisdom | Charisma | NoAbilityType deriving (Show) 
data Dice = D2 | D3 | D4 | D6 | D8 | D10 | D12 | D20 | D Int | FlatMod Int deriving (Show) 
type DiceTotal = (Int, Dice, DamageType)


type DmgModifiers = (String, Int)

data DamageAssistant = DA { dice :: [DiceTotal]
                          , abil :: [AbilityType] 
                          , flatBonuses :: [DmgModifiers]
                          }  deriving (Show) 
instance Monoid DamageAssistant where
    mappend (DA a b c) (DA x y z) = DA (a <> x) (b <> y) (c <> z)
    mempty = DA [] [] []

type Description = String 

data MetaAttack = MetaAtk { addDmg           :: [DiceTotal]
                          , addResourceDrain :: [ResourceDrain] 
                          , descriptionMeta  :: [Description]
                          } deriving (Show) 
instance Monoid MetaAttack where
    mappend (MetaAtk a b c) (MetaAtk x y z) = MetaAtk (a <> x) (b <> y) (c <> z)
    mempty = MetaAtk [] [] []

type Score = Int
type TempScoreMod = Int
type AbilityScore  = (AbilityType, Score, TempScoreMod)

data Player = Plr { strScore  :: AbilityScore
                  , dexScore  :: AbilityScore
                  , intScore  :: AbilityScore
                  , conScore  :: AbilityScore
                  , wisScore  :: AbilityScore
                  , charScore :: AbilityScore 
                  } deriving (Show) 

data Attack = Atk { meta :: [MetaAttack]
                  , dmg  :: [DiceTotal]
                  , descriptionAtk :: [Description]
                  } deriving (Show) 
instance Monoid Attack where
    mappend (Atk a b c) (Atk x y z) = Atk (a <> x) (b <> y) (c <> z)
    mempty = Atk [] [] []


-- Tools 
tryAll2Combos :: Monoid a => [a] -> [a] -> [a]
tryAll2Combos [] ys = []
tryAll2Combos xs [] = []
tryAll2Combos (x:xs) ys = (map (x<>) ys) ++ (tryAll2Combos xs ys)


-- Examples 
makeSingleMeta x y z = MetaAtk [x] [y] [z]
makeDmgHelper x y z = DA [x] [y] [z]
makeSingleAttack di m de = Atk [m] [di] [de]

fireMeta = makeSingleMeta moreDmg moreDrain moreInfo
    where 
        moreDmg = (3,D4,Fire)
        moreDrain = SpellLevelIncrease 1
        moreInfo = "Knocks target prone too"
        
acidMeta = makeSingleMeta moreDmg moreDrain moreInfo
    where 
        moreDmg = (6,D6,Acid)
        moreDrain = SpellLevelIncrease 1
        moreInfo = "Adds Acid dmg"
        
coldMeta = makeSingleMeta moreDmg moreDrain moreInfo
    where 
        moreDmg = (3,D 42,Cold)
        moreDrain = ManaRequirement (-51)
        moreInfo = "Foo"
        
slashingeMeta = makeSingleMeta moreDmg moreDrain moreInfo
    where 
        moreDmg = (2,D20,Slashing)
        moreDrain = ManaRequirement 1
        moreInfo = "Baro"

makeNewMeta = tryAll2Combos metaList metaList
    where 
        metaList = [fireMeta,acidMeta,coldMeta,slashingeMeta]



atk1 = makeSingleAttack moreDmg fireMeta moreInfo
    where 
        moreDmg = (3,D4,Bludgeoning)
        moreInfo = "Knocks target prone too"
        
atk2 = makeSingleAttack moreDmg slashingeMeta moreInfo
    where 
        moreDmg = (6,D6,Slashing)
        moreInfo = "Adds Acid dmg"
        
atk3 = makeSingleAttack moreDmg acidMeta moreInfo
    where 
        moreDmg = (3,D 42, NoType)
        moreInfo = "Atk Foo"
        
atk4 = makeSingleAttack moreDmg (coldMeta<>coldMeta) moreInfo
    where 
        moreDmg = (5,D20,Slashing)
        moreInfo = "Atk4"

makeNewAtks = tryAll2Combos metaList metaList
    where 
        metaList = [atk1,atk3,atk3,atk4]







-- data PhysicalAttack = PAtk{ physicalDmg     :: [DamageType]
--                           , phyEnergyDmg    :: [DamageType]
--                           , physicalDmgHelp :: [DamageAssistant] }
-- instance Monoid PhysicalAttack where
--     mappend (PAtk a b c) (PAtk x y z) = PAtk (a <> x) (b <> y) (c <> z)
--     mempty = PAtk [] [] []

-- data MagicAttack = MAtk { magicDmg     :: [PhysicalType]
--                         , magEnergyDmg :: [EnergyType]
--                         , magicDmgHelp :: [DamageAssistant] }
-- instance Monoid MagicAttack where
--     mappend (MAtk a b c) (MAtk x y z) = MAtk (a <> x) (b <> y) (c <> z)
--     mempty = MAtk [] [] []
