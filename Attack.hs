{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

import Prelude
import Data.Monoid

import qualified Text.PrettyPrint.HughesPJClass as TPH


main = undefined 


--Pretty Print functions-----------------------------------

--Helpful pretty print renames
txt = TPH.text
(<<+>>) = (TPH.<+>)
(<<>>) = (TPH.<>)
($$$$) = (TPH.$$)
($$+$$) = (TPH.$+$)
pPrint :: forall a. TPH.Pretty a => a -> TPH.Doc
pPrint = TPH.pPrint
nest = TPH.nest

shxt :: Show a => a -> TPH.Doc
shxt = TPH.text . show

mapListShowDoc :: Show a => [a] -> TPH.Doc
mapListShowDoc = TPH.sep.map shxt

mapListDoc :: TPH.Pretty a => [a] -> TPH.Doc
mapListDoc = TPH.sep.map pPrint

--End Pretty Print functions-------------------------------


--Types ---------------------------------------------------
data ResourceDrain = SpellLevelIncrease Int | ManaRequirement Int deriving (Show) 
data DamageType = Piercing | Bludgeoning | Slashing | Fire | Acid | Cold | Lightning | NoType deriving (Show) 
data AbilityType = Strength | Dexterity | Intelligence | Constitution | Wisdom | Charisma | NoAbilityType deriving (Show) 
data Dice = D2 | D3 | D4 | D6 | D8 | D10 | D12 | D20 | D Int | FlatMod Int deriving (Show) 

type Name = String
type Description = String 
type DmgModifiers = (Int, Description)
type DiceCount = Int

data DiceTotal = DT { dc   :: DiceCount 
                    , die  :: Dice 
                    , dt   :: DamageType 
                    , desc :: Description } deriving (Show) 
instance TPH.Pretty DiceTotal where
  pPrint DT{..} = txt "Damage dice:" <<+>> (shxt dc <<>> txt "x" <<>> shxt die)
            $$+$$ txt "Damage is of type:" <<+>> shxt dt
            $$+$$ txt  "Dice from:" <<+>> txt desc

data DamageAssistant = DA { dice :: [DiceTotal]
                          , abil :: [AbilityType] 
                          , flatBonuses :: [DmgModifiers]
                          }  deriving (Show)                          
instance Monoid DamageAssistant where
    mappend (DA a b c) (DA x y z) = DA (a <> x) (b <> y) (c <> z)
    mempty = DA [] [] []
instance TPH.Pretty DamageAssistant where
  pPrint DA{..} = txt "Attack total damage dice:" $$+$$ mapListDoc dice
            $$+$$ txt "Attack uses:" $$+$$ mapListShowDoc abil
            $$+$$ txt "Attack has following flat bonus(es):" $$+$$ mapListDoc flatBonuses


data Attack = Atk { atkName :: Name
                  , meta :: [MetaAttack]
                  , dmg  :: [DiceTotal]
                  , descriptionAtk :: [Description]
                  } deriving (Show) 
instance TPH.Pretty Attack where
  pPrint Atk{..} = txt "Attack has name:" <<+>> shxt atkName
             $$+$$ nest 2 (
                     nest 2 ( txt "Attack has base damage:" $$+$$ nest 2 (mapListDoc dmg)
                     $$+$$ txt "Attack has base description:" <<+>> nest 2 (mapListDoc descriptionAtk)
                     $$+$$ txt "Attack has modifers:" $$+$$ nest 2 (mapListDoc meta)))
instance Monoid Attack where
    mappend (Atk n1 a b c) (Atk n2 x y z) = Atk (n1 <> "/" <> n1) (a <> x) (b <> y) (c <> z)
    mempty = Atk "" [] [] []

data MetaAttack = MetaAtk { maName           :: Name
                          , addDmg           :: [DiceTotal]
                          , addResourceDrain :: [ResourceDrain] 
                          , descriptionMeta  :: [Description]
                          } deriving (Show) 
instance Monoid MetaAttack where
    mappend (MetaAtk n1 a b c) (MetaAtk n2 x y z) = MetaAtk (n1 <> "/" <> n1) (a <> x) (b <> y) (c <> z)
    mempty = MetaAtk "" [] [] []
instance TPH.Pretty MetaAttack where
  pPrint MetaAtk{..} = txt "Modifer Name:" <<+>> shxt maName             
                 $$+$$ nest 2 (
                          nest 2 ( txt "Attack does addtional:" $$+$$ nest 2 (mapListDoc addDmg)
                          $$+$$ txt "Attack uses addtional:" $$+$$ nest 2 (mapListShowDoc addResourceDrain)
                          $$+$$ txt "Attack has addtional description:" $$+$$ nest 2 (mapListDoc descriptionMeta)))

type Score = Int
type TempScoreMod = Int
type AbilityScore  = (AbilityType, Score, TempScoreMod)


data Player = Plr { plyName   :: Name
                  , strScore  :: AbilityScore
                  , dexScore  :: AbilityScore
                  , intScore  :: AbilityScore
                  , conScore  :: AbilityScore
                  , wisScore  :: AbilityScore
                  , charScore :: AbilityScore 
                  } deriving (Show) 
instance TPH.Pretty Player where
  pPrint Plr{..} = txt "Player has stats:"
             $$+$$ txt "Strength Score:" <<+>> shxt strScore 
             $$+$$ txt "Dexterity Score:" <<+>> shxt dexScore 
             $$+$$ txt "Intelligence Score:" <<+>> shxt intScore 
             $$+$$ txt "Constitution Score:" <<+>> shxt conScore 
             $$+$$ txt "Wisdom Score:" <<+>> shxt wisScore 
             $$+$$ txt "Charisma Score:" <<+>> shxt charScore


-- Tools 
tryAll2Combos :: Monoid a => [a] -> [a] -> [a]
tryAll2Combos [] ys = []
tryAll2Combos xs [] = []
tryAll2Combos (x:xs) ys = (map (x<>) ys) ++ (tryAll2Combos xs ys)


-- Examples 
makeSingleMeta n x y z = MetaAtk n [x] [y] [z]
makeDmgHelper x y z = DA [x] [y] [z]
makeSingleAttack n di m de = Atk n [m] [di] [de]

fireMeta = makeSingleMeta name moreDmg moreDrain moreInfo
    where
        name = "Fire Bonus" 
        moreDmg = DT 3 D4 Fire "fire extra"
        moreDrain = SpellLevelIncrease 1
        moreInfo = "Knocks target prone too"
        
acidMeta = makeSingleMeta name moreDmg moreDrain moreInfo
    where 
        name = "Acid Bonus" 
        moreDmg = DT 6 D6 Acid "acid extra"
        moreDrain = SpellLevelIncrease 1
        moreInfo = "Adds Acid dmg"
        
coldMeta = makeSingleMeta name moreDmg moreDrain moreInfo
    where 
        name = "Cold Bonus" 
        moreDmg = DT 3 (D 42) Cold "meta cold thing"
        moreDrain = ManaRequirement (-51)
        moreInfo = "Foo"
        
slashingeMeta = makeSingleMeta name moreDmg moreDrain moreInfo
    where 
        name = "Slashing Bonus" 
        moreDmg = DT 2 D20 Slashing "meta slash"
        moreDrain = ManaRequirement 1
        moreInfo = "Baro"

makeNewMeta = tryAll2Combos metaList metaList
    where 
        metaList = [fireMeta,acidMeta,coldMeta,slashingeMeta]



atk1 = makeSingleAttack name moreDmg fireMeta moreInfo
    where 
        name = "Bludgeoning Bonus" 
        moreDmg = DT 3 D4 Bludgeoning "Bludgeoning thing"
        moreInfo = "Knocks target prone too"
        
atk2 = makeSingleAttack name moreDmg slashingeMeta moreInfo
    where 
        name = "Slashing Attack" 
        moreDmg = DT 6 D6 Slashing "desc of Slashing"
        moreInfo = "Adds Acid dmg"
        
atk3 = makeSingleAttack name moreDmg acidMeta moreInfo
    where 
        name = "Small Acid" 
        moreDmg = DT 3 (D 42)  NoType "a thing of acid"
        moreInfo = "Atk Foo"
        
atk4 = makeSingleAttack name moreDmg (coldMeta<>coldMeta) moreInfo
    where 
        name = "Big Slash" 
        moreDmg = DT 5 D20 Slashing "FOOOOBAAAARRR"
        moreInfo = "Atk4"

makeNewAtks = tryAll2Combos metaList metaList
    where 
        metaList = [atk1,atk3,atk3,atk4]

