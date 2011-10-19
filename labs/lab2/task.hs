import Test.QuickCheck
import Data.List (permutations, sort)
import Control.Monad(liftM2)
import Control.Exception (assert)
import Data.Bits (shift)
import Data.Ix (inRange)

type LHV = Int

newtype Point = Point (Int,Int)
  deriving (Show)

newtype Rect = Rect (Point,Point)
  deriving (Show)

x,y :: Point -> Int
x (Point (a,b)) = a
y (Point (a,b)) = b

-- bottom-left + top-right
bl,tr :: Rect -> Point
bl (Rect (p1,p2)) = p1
tr (Rect (p1,p2)) = p2

blx,bly,trx,try :: Rect -> Int
blx = x . bl
bly = y . bl
trx = x . tr
try = y . tr

size :: Rect -> Int
size r = (sizex == sizey) `assert` sizex
  where
    sizex = trx r - blx r
    sizey = try r - bly r

instance Arbitrary Point where
  arbitrary = liftM2 (curry Point) arbitrary arbitrary

instance Arbitrary Rect where
  arbitrary = liftM2 mkRect arbitrary arbitrary

mkRect :: Point -> Int -> Rect
mkRect p1 size = Rect (p1,b)
  where
    b = Point (x p1 + size, y p1 + size)


class Foldable a where
  folded :: a -> Rect -> Bool

instance Foldable Point where
  folded p r = xfit && yfit
    where
      xfit = inRange (blx r, trx r) $ x p 
      yfit = inRange (bly r, try r) $ y p

instance Foldable Rect where
  folded r1 r2 = bl r1 `folded` r2 && tr r1 `folded` r2

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

p0 = Point (0,0)
p1 = Point (1,1)
p8 = Point (8,8)
r8 = Rect (p0, p8)
r1 = Rect (p0, p1)

get_quoters :: Rect -> [Rect]
get_quoters (Rect (Point(x1,y1), Point(x2,y2))) = [r0, r1, r2, r3]
  where
    xm = (x1 + x2) `div` 2
    ym = (y1 + y2) `div` 2
    r0 = Rect (Point(x1,y1), Point(xm,ym))
    r1 = Rect (Point(x1,ym), Point(xm,y2))
    r2 = Rect (Point(xm,ym), Point(x2,y2))
    r3 = Rect (Point(xm,y1), Point(x2,ym))

prop_folded_rect :: Rect -> Rect -> Bool
prop_folded_rect r1 r2 = r1 `folded` r2 == myFolded 
  where
    x_folded = blx r1 >= blx r2 && trx r1 <= trx r2
    y_folded = bly r1 >= bly r2 && try r1 <= try r2
    myFolded = x_folded && y_folded

prop_point_in_one_quoter :: Point -> Rect -> Property
prop_point_in_one_quoter point rect = 
  (size rect >= 2) ==> length qs_with_point <= 1
  where
    qs = get_quoters rect
    qs_with_point = filter (\q -> point `folded` q) qs
  
  
  

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


