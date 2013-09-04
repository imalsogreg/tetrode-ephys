module Data.Ephys.OldMWL.FileInfo where
       
--------------------------------------------------------------------------------
-- FileHeader.hs 
--
-- Parse enough of MWL file headers to extract data from them in arte format
-- (eg - need to get channel gains, possibly thresholds, record formatting)
--
-- MWL headers are a little messy because MWL files go through a couple
-- processing steps, with intermediate data saved in intermediate files,
-- and each file lists the headers of the files of the preceding steps
--
-- I'll mostly ignore this, and try to tailor the header-loading to a
-- couple of file types, selectively ignoring parts of the total
-- header that I know aren't needed for specific uses of the file.
-- (eg - header of a tt file will discard the 'original ad file' field.)
--
-- AD files and tt files have a different format for the 'fields' field.
-- I may not have dealt with this the right way for all types of files.
--------------------------------------------------------------------------------

import Control.Monad
import Control.Applicative ((<$>),(<*>))
import Data.Traversable (traverse)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
--import System.IO.Unsafe
import Data.List (isInfixOf,isSuffixOf)
import Data.Map as Map

data FileType = Binary | Ascii deriving (Eq, Show, Read)

data RecordMode = Spike | Continuous deriving (Eq, Show)

readRecordMode :: String -> RecordMode
readRecordMode "CONTINUOUS" = Continuous
readRecordMode "SPIKE"      = Spike

data ExtractionType = TetrodeWaveforms deriving (Eq, Show)

readExtractionType :: String -> ExtractionType
readExtractionType "tetrode waveforms" = TetrodeWaveforms

type DatumName = String

type RecordDescr = (DatumName, DatumType, DatumRepeatCount)
data ChanDescr = ChanDescr {ampGain    :: Double
                           ,adGain     :: Double
                           ,filterCode :: Integer
                           ,threshold  :: Double
                           ,colorCode  :: Integer
                           }
                 deriving (Eq, Show)

data FileInfo = FileInfo { hProgram     :: String
                         , hVersion     :: String
                         , hArgv        :: [String]
                         , hDate        :: String
                         , hDir         :: String
                         , hHostname    :: String
                         , hArch        :: String
                         , hUser        :: String
                         , hFileType    :: FileType
                         , hExtractT    :: ExtractionType
                         , hProbe       :: Integer
                         , hRecordDescr :: [RecordDescr]
                         , hRate        :: Double
                         , hNTrodes     :: Integer
                         , hNChans      :: Integer
                         , hNTrodeChans :: Integer
                         , hRecMode     :: RecordMode
                         , hChanDescrs  :: [ChanDescr]
                         } deriving (Eq, Show)

type FileName = String

parseFields :: CharParser () [RecordDescr]
parseFields = parseField `sepBy` (char '\t' <|> char ' ')

parseField :: CharParser () RecordDescr
parseField = do
  fieldName <- many (noneOf ",")
  _  <- many digit  -- We ignore the size field from the file b/c it's fixed by the type
  char ','
  datumCode  <- many digit
  char ','
  datumCount <- many digit
  return (fieldName, datumTypeFromIntegral (read datumCode), read datumCount)

getFileInfo :: FileName -> IO FileInfo
getFileInfo fn = do
  c   <- readFile fn
  let pMap = paramStringsMap $ parse pFileHeader "MWL File Header" c
      grab k = case Map.lookup k pMap of
        Just v  -> Right v
        Nothing -> Left $ "Error getting field " ++ show k
      nChans = fromIntegral . length $ Prelude.filter ("threshold" `isSuffixOf`) (Map.keys pMap)
      progNArg = read $ either (const "0") (id) $ grab "Argc" :: Integer
      mFileInfo = FileInfo
                  <$> grab "Program"
                  <*> grab "Program Version"
                  <*> traverse grab ["Argv[" ++ show n ++ "]" | n <- [1..progNArg - 1]]
                  <*> grab "Date"
                  <*> grab "Directory"
                  <*> grab "Hostname"
                  <*> grab "Architecture"
                  <*> grab "User"
                  <*> read `liftM` grab "File type"
                  <*> readExtractionType `liftM` grab "Extraction type"
                  <*> read `liftM` grab "Probe"
                  <*> either (Left . show) (Right . id)  -- There must be a better way than this.
                  (parse parseFields "MWL Header Fields" (either (const "") id $ grab "Fields"))

                  <*> read `liftM` grab "rate"
                  <*> read `liftM` grab "nelectrodes"
                  <*> read `liftM` grab "nchannels"
                  <*> read `liftM` grab "nelect_chan"
                  <*> readRecordMode `liftM` grab "mode"
                  <*> traverse (grabChannelDescr pMap) [0 .. nChans - 1] :: Either String FileInfo

  case mFileInfo of
    Left e -> error $ "Error getting FileInfo: " ++ e
    Right fi -> return fi

grabChannelDescr :: Map String String -> Integer -> Either String ChanDescr
grabChannelDescr m n = let grab k = case Map.lookup k m of
                             Just v  -> Right v
                             Nothing -> Left $ "ChannelDescr error grabbing field " ++ k
                       in
  do
  ampGain  <- grab (unwords ["channel", show n, "ampgain"])
  adGain   <- grab (unwords ["channel", show n, "adgain"])
  filtCode <- grab (unwords ["channel", show n, "filter"])
  thresh   <- grab (unwords ["channel", show n, "threshold"])
  col      <- grab (unwords ["channel", show n, "color"])
  return $ ChanDescr (read ampGain) (read adGain) (read filtCode) (read thresh) (read col)

type DatumRepeatCount = Integer
data DatumType = DInvalid
               | DChar
               | DShort
               | DInt
               | DFloat
               | DDouble
               | DFunc
               | DFFunc
               | DULong
               | DUnknown
               deriving (Eq, Ord, Show)

datumTypeIntMap :: [(DatumType, Integer)]
datumTypeIntMap = [(DInvalid, 0),(DChar, 1),(DShort,2),(DInt,3)
                  ,(DFloat,4),(DDouble,5),(DFunc,6),(DFFunc,7)
                  ,(DULong,8),(DUnknown,-1)]
datumTypeToIntegral :: DatumType -> Integer
datumTypeToIntegral d = maybe (-1) id (Prelude.lookup d datumTypeIntMap)
datumTypeFromIntegral :: Integer -> DatumType
datumTypeFromIntegral i =
  maybe DUnknown id (Prelude.lookup i (Prelude.map(\(a,b)->(b,a)) datumTypeIntMap))


-- Parsec parsing of MWL Header
pFileHeader :: CharParser () [(String, Maybe String)]
pFileHeader = do
  string "%%BEGINHEADER\n"
  pairs <- many pHeaderLine
  string "%%ENDHEADER\n"
  return pairs

pHeaderLine :: CharParser () (String, Maybe String)
pHeaderLine = do
  entry <- try pPair
           <|> try pUnusedLine
           <|> try pMalformedPair
           <?> "unparsable line"
  return entry

pPair :: CharParser () (String, Maybe String)
pPair = do
  string "% "
  spaces
  name <- many pKeyChar
  value <- optionMaybe (pKVSep >> many pValChar)
  char '\n'
  return (stripTrailingSpaces name, value)

stripTrailingSpaces :: String -> String
stripTrailingSpaces s = reverse . dropWhile (== ' ') . reverse $ s

pMalformedPair :: CharParser () (String, Maybe String)
pMalformedPair = do
  string "%"
  spaces
  name <- many (noneOf "%:\n")
  char '\n'
  return (name,Nothing)

pUnusedLine :: CharParser () (String, Maybe String)
pUnusedLine = do
  string "%\n"
  return ("",Nothing)

pKVSep :: CharParser () [Char]
pKVSep = char ':' >> many (char ' ' <|> char '\t')

pKeyChar :: CharParser () Char
pKeyChar = noneOf ":\t\n"

pValChar :: CharParser () Char
pValChar = noneOf "\n"

paramStringsMap :: Either ParseError [(String, Maybe String)] -> Map.Map String String
paramStringsMap (Left e) = error $ "Header parse error for MWL file: " ++ show e
paramStringsMap (Right as) = Prelude.foldl insJustsWithoutReplacement Map.empty as
  where
    insJustsWithoutReplacement m (_, Nothing) = m
    insJustsWithoutReplacement m (k, Just v)  =
      case Map.lookup k m of
        Nothing  -> insert k v m
        (Just _) -> m

