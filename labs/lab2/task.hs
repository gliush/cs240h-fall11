import Data.List(sort)
import Test.QuickCheck
import Data.List (permutations)
import Control.Exception (assert)

type LHV = Int
newtype Point = Point (Int,Int)
  deriving (Show)

newtype Rect = Rect (Point,Point)
  deriving (Show)

class Foldable a where
  folded :: a -> Rect -> Bool

instance Foldable Point where
  folded (Point (x,y)) (Rect (Point (x1,y1),Point (x2,y2))) = xfit && yfit
    where
      xfit = x >= x1 && x <= x2
      yfit = y >= y1 && y <= y2

instance Foldable Rect where
  folded (Rect (p1,p2))  rect = p1 `folded` rect && p2 `folded` rect

type Obj = Rect
type MBR = Rect

-- nodes 
type EntriesCount = Int
data TreeNode = TreeNode_l EntriesCount [(MBR,Obj)] | 
                TreeNode_n EntriesCount [(MBR,LHV,TreeNode)]
  deriving (Show)

newtype HilbertTree = HilbertTree TreeNode
  deriving (Show)


main = undefined

-- sample generate funs
gen_rect = Rect (Point (0,0),Point (5,5))
gen_nl = TreeNode_l 1 [(gen_rect,gen_rect)]
gen_nn = TreeNode_n 1 [(gen_rect,5,gen_nl)]

insert :: HilbertTree -> (MBR,Point,Obj) -> HilbertTree
insert tree (mbr, center, obj) = undefined

select :: HilbertTree -> Obj -> [TreeNode]
select = undefined

calc_lhv :: Rect -> Point -> LHV
calc_lhv = undefined

-- http://blog.notdot.net/2009/11/Damn-Cool-Algorithms-Spatial-indexing-with-Quadtrees-and-Hilbert-Curves

{-
point_to_lhv :: Rect -> Rect -> Point -> LHV
point_to_lhv whole cur (x,y)@point = 
  let Rect (curx1,cury1) (curx2,cury2) = cur
      nextx = assert 

      next_cur = assert (and [point `in` cur, cur `in` whole]) next

-}
