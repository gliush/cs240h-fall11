import Data.List(sort)
import Test.QuickCheck
import Data.List (permutations)
import Control.Exception (assert)
import Data.Bits (shift)

type LHV = Int
newtype Point = Point (Int,Int)
  deriving (Show)

newtype Rect = Rect (Point,Point)
  deriving (Show)

class Foldable a where
  folded :: a -> Rect -> Bool

size :: Rect -> Int
size (Rect (p1,p2)) = x2 - x1
  where 
    Point (x1,_) = p1
    Point (x2,_) = p2

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

r8 = Rect (Point(0,0), Point(8,8))
r1 = Rect (Point(0,0), Point(1,1))

get_quoters :: Rect -> [Rect]
get_quoters (Rect (Point(x1,y1), Point(x2,y2))) = [r0, r1, r2, r3]
  where
    xm = (x1 + x2) `div` 2
    ym = (y1 + y2) `div` 2
    r0 = Rect (Point(x1,y1), Point(xm,ym))
    r1 = Rect (Point(x1,ym), Point(xm,y2))
    r2 = Rect (Point(xm,ym), Point(x2,y2))
    r3 = Rect (Point(xm,y1), Point(x2,ym))

prop_point_in_one_quoter :: Point -> Rect -> Bool
prop_point_in_one_quoter point rect = undefined
  
  
  

get_quoter :: Int -> Rect -> Rect
get_quoter n rect = get_quoters rect !! n

{-
point_to_lhv :: Rect -> Rect -> Point -> LHV
point_to_lhv whole cur point | size cur == 1 = 1
                             | otherwise = shift ind 2 * nextv
  where
    qs = (zip [0..] $ get_quoters cur) :: [(Int, Rect)]
    [(ind, q)] = filter (\(_,r) -> point `folded` r) qs
    next_cur = assert (and [point `folded` cur, cur `folded` whole]) q
    nextv = point_to_lhv whole next_cur point 
    -}


