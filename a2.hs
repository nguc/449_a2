--
-- CPSC 449 Assignment 2 Starter Code
--
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits
import Data.Char
import Codec.Compression.Zlib as Z
import Numeric (showHex)

--
-- Define your algebraic types for Part 1 here
--
-- // ===================== Part 1 ==================== //
--
-- Quad Tree Data Type
-- Leaf (Int, Int, Int) = (Red, Blue, Green)
-- Node (QuadTree, Quadtree, QuadTree, QuadTree) = (TL, TR, BL, BR)
data QuadTree = Leaf Int Int Int | Node QuadTree QuadTree QuadTree QuadTree | NilT

-- QuadTreeImage Data Type
-- SquareImg (Int, QuadTree) = (width, QuadTree)
data Image = SquareImg Int QuadTree

-- Leaf constructor for QuadTree
newLeaf :: (Int, Int, Int) -> QuadTree
newLeaf (r,g,b)
  | r < 0 || g < 0 || b < 0 || r > 255 || g > 255 || b > 255 = 
        error "Invalid values. Values must be between 0 and 255"
  | otherwise = (Leaf r g b)

instance Show QuadTree where
  show (Leaf r g b) = "Leaf: (" ++ (show r) ++ ", " ++ (show g) ++ ", " ++ (show b) ++ ") "
  show (Node a b c d) = "Node: [ " ++ (show a) ++ (show b) ++ (show c) ++ (show d) ++ "] "
  show NilT = "[ ]"

instance Show Image where
  show (SquareImg w qt) = "QuadTree Image (Width = " ++ (show w) ++ "): " ++ (show qt)

-- // ===================================================== //


--
-- The following functions are a simple PNG file loader.  Note that these
-- functions will not load all PNG files.  They makes some assumptions about
-- the structure of the file that are not required by the PNG standard.
--

--
-- Convert 4 8-bit words to a 32 bit (or larger) integer
--
make32Int :: Word8 -> Word8 -> Word8 -> Word8 -> Int 
make32Int a b c d = ((((fromIntegral a) * 256) + 
                       (fromIntegral b) * 256) + 
                       (fromIntegral c) * 256) + 
                       (fromIntegral d)

--
-- Get a list of all of the PNG blocks out of a list of bytes
--
getBlocks :: [Word8] -> [(String, [Word8])]
getBlocks [] = []
getBlocks (a:b:c:d:e:f:g:h:xs) = (name, take (size+12) (a:b:c:d:e:f:g:h:xs)) : getBlocks (drop (size + 4) xs)
  where
    size = make32Int a b c d
    name = (chr (fromIntegral e)) : (chr (fromIntegral f)) :
           (chr (fromIntegral g)) : (chr (fromIntegral h)) : []

--
-- Extract the information out of the IHDR block
--
getIHDRInfo :: [(String, [Word8])] -> (Int, Int, Int, Int)
getIHDRInfo [] = error "No IHDR block found"
getIHDRInfo (("IHDR", (_:_:_:_:_:_:_:_:w1:w2:w3:w4:h1:h2:h3:h4:bd:ct:_)) : _) = (make32Int w1 w2 w3 w4, make32Int h1 h2 h3 h4, fromIntegral bd, fromIntegral ct)
getIHDRInfo (x : xs) = getIHDRInfo xs

--
-- Extract and decompress the data in the IDAT block.  Note that this function
-- only handles a single IDAT block, but the PNG standard permits multiple
-- IDAT blocks.
--
getImageData :: [(String, [Word8])] -> [Word8]
getImageData [] = error "No IDAT block found"
getImageData (("IDAT", (_:_:_:_:_:_:_:_:xs)) : _) = BS.unpack (Z.decompress (BS.pack (take (length xs - 4) xs)))
getImageData (x:xs) = getImageData xs

--
-- Convert a list of bytes to a list of color tuples
--
makeTuples :: [Word8] -> [(Int, Int, Int)]
makeTuples [] = []
makeTuples (x : y : z : vals) = (fromIntegral x, fromIntegral y, fromIntegral z) : makeTuples vals

--
-- Convert a list of bytes that have been decompressed from a PNG file into
-- a two dimensional list representation of the image
--
imageBytesToImageList :: [Word8] -> Int -> [[(Int, Int, Int)]]
imageBytesToImageList [] _ = []
imageBytesToImageList (_:xs) w = makeTuples (take (w * 3) xs) : imageBytesToImageList (drop (w * 3) xs) w

--
-- Determine how many IDAT blocks are in the PNG file
--
numIDAT :: [(String, [Word8])] -> Int
numIDAT vals = length (filter (\(name, dat) -> name == "IDAT") vals)

--
-- Convert the entire contents of a PNG file (as a ByteString) into 
-- a two dimensional list representation of the image
--
decodeImage :: BS.ByteString -> [[(Int, Int, Int)]]
decodeImage bytes
  | header == [137,80,78,71,13,10,26,10] &&
    colorType == 2 &&
    bitDepth == 8 = imageBytesToImageList imageBytes w
  | numIDAT blocks > 1 = error "The image contained too many IDAT blocks"
  | otherwise = error ("Invalid header\ncolorType: " ++ (show colorType) ++ "\nbitDepth: " ++ (show bitDepth) ++ "\n")
  where
    header = take 8 $ BS.unpack bytes
    (w, h, bitDepth, colorType) = getIHDRInfo blocks
    imageBytes = getImageData blocks
    blocks = getBlocks (drop 8 $ BS.unpack bytes)

--
-- Insert your code here for Parts 2, 3 and 4 here
--
-- 
-- // ===================== Part 2 ==================== //

-- Creates a quad tree image from a 2D list of tuples
createTree :: [[(Int, Int, Int)]] -> Image
createTree list
  | (isSquare list) = SquareImg (length list) (makeQuadTree list)
  | otherwise = error "The image is not a square"


-- returns true if the 2D list is a square
isSquare :: [[a]] -> Bool
isSquare [] = True
isSquare list
  | (length list) == (length (head list)) = True
  | otherwise = False

-- creates a quad tree from a 2D list representation of an image
makeQuadTree :: [[(Int, Int, Int)]] -> QuadTree
makeQuadTree [[]] = NilT
makeQuadTree [] = NilT
makeQuadTree arr
  | samePixels arr = newLeaf (head (head arr))
  | otherwise  = Node (makeQuadTree (quarterArray arr 1)) 
                      (makeQuadTree (quarterArray arr 2)) 
                      (makeQuadTree (quarterArray arr 3)) 
                      (makeQuadTree (quarterArray arr 4))


samePixels :: [[(Int, Int, Int)]] -> Bool
samePixes [] = True
samePixels [x] = isAllSame x
samePixels (x:xs)
   | isAllSame x && x == (head xs) = samePixels xs
   | otherwise = False


-- returns true if all values in the list are the same
isAllSame :: Eq a => [a] -> Bool
isAllSame [] = True
isAllSame [x] = True
isAllSame (x:xs)
  | x == (head xs) = (isAllSame xs)
  | otherwise = False


-- Takes a 2D list and returns a quarter of the list. 
-- The Int input should be either 1, 2, 3, or 4, 
-- where  1: Top left, 2: Top Right, 3: Bottom Left, 4: Bottmo Right
quarterArray :: [[a]] -> Int -> [[a]]
quarterArray arr i
  | i == 1 = frontHalfList (frontHalfArray arr)
  | i == 2 = frontHalfList (backHalfArray arr)
  | i == 3 = backHalfList (frontHalfArray arr)
  | i == 4 = backHalfList (backHalfArray arr)
  | otherwise = error "quarterArray takes an input of [[a]]"

-- Function that takes a list of lists, and halves each list, returning the front half.
frontHalfArray :: [[a]] -> [[a]]
frontHalfArray [] = []
frontHalfArray (x:xs) = (frontHalfList x):(frontHalfArray xs)
--

-- Function which takes a list of lists, and halves each list, returning the back half.
backHalfArray :: [[a]] -> [[a]]
backHalfArray [] = []
backHalfArray (x:xs) = (backHalfList x):(backHalfArray xs)
--

-- Function which takes a list, and returns the front half of it.
frontHalfList :: [a] -> [a]
fronttHalfList [] = []
frontHalfList xs
   | (length xs) `rem` 2 == 0 = take ((length xs) `div` 2) xs
   | otherwise = take ((length xs)`div` 2 + 1) xs
--

-- Function which takes a list, and returns the back half of it.
backHalfList :: [a] -> [a]
backHalfList [] = []
backHalfList xs
   | (length xs) `rem` 2 == 0 = drop ((length xs) `div` 2) xs
   | otherwise = drop ((length xs) `div` 2 +1) xs   
--


-- // ================================================= //


-- // ===================== Part 3 ==================== //

-- // ================================================= //

-- // ===================== Part 4 ==================== //

-- // ================================================= //
--
-- Load a PNG file, convert it to a quad tree, mirror it, grayscale it,
-- and write all three images to an .html file.
--
main :: IO ()
main = do
  -- Change the name inside double quotes to load a different file
  -- input <- BS.readFile "Mondrian.png"
  input <- BS.readFile "Mondrian.png"
 
  -- image is the list representation of the image stored in the .png file
  let image = decodeImage input

  -- Convert the list representation of the image into a tree representation
  let qtree_image = createTree image

  -- Gray scale the tree representation of the image
  --let gs = grayscale qtree_image

  -- Mirror the tree representation of the image
  --let mi = mirror qtree_image
 
  -- Write the original, mirrored and grayscale images to quadtree.html
  writeFile "quadtree.html" "" -- take this line out and use the lines below 
                               -- instead once you have your functions written

  --writeFile "quadtree.html" ((toHTML qtree_image) ++ "<br><br><br>" ++ 
  --                           (toHTML gs) ++ "<br><br><br>" ++
  --                           (toHTML mi) ++ "<br><br><br>")