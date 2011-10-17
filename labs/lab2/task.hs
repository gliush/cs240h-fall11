import Data.List(sort)
import Test.QuickCheck
import Data.List (permutations)

type LHV = Int
type Point = (Int,Int) 
data Rect = Rect Point Point 
  deriving (Show)


type Obj = Rect

-- nodes 
type EntriesCount = Int
data TreeNode = TreeNode_l EntriesCount [(Rect,Obj)] | 
                TreeNode_n EntriesCount [(Rect,LHV,TreeNode)]
  deriving (Show)

newtype HilbertTree = HilbertTree TreeNode
  deriving (Show)


main = undefined

-- sample generate funs
gen_rect = Rect (0,0) (5,5)
gen_nl = TreeNode_l 1 [(gen_rect,gen_rect)]
gen_nn = TreeNode_n 1 [(gen_rect,5,gen_nl)]
