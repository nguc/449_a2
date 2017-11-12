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
data QuadTree = Leaf (Int, Int, Int) | InnerNode QuadTree QuadTree QuadTree QuadTree
data QuadTreeImage = NoImg | QuadTreeImage Int QuadTree
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
-- Part 2 code
createTree :: [[(Int, Int, Int)]] -> QuadTreeImage
createTree list
  | isImgSquare list == False = error "The image is not square" 
  | checkColour
  | otherwise = NoImg

isImgSquare :: [[(Int, Int, Int)]] -> Bool
isImgSquare list = all (\x -> length x == length list) list

checkColour :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
checkColour (r1, g1, b1) (r2, g2, b2)
  | r1 == r2 && g1 == g2 && b1 == b2 = True
  | otherwise = False

--
-- Load a PNG file, convert it to a quad tree, mirror it, grayscale it,
-- and write all three images to an .html file.
--
main :: IO ()
main = do
  -- Change the name inside double quotes to load a different file
  -- input <- BS.readFile "Mondrian.png"
  input <- BS.readFile "notsq.png"
 
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