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

--import Text.Parsec

data FileType = Binary | Ascii deriving (Eq, Show)
data RecordMode = Spike | Continuous deriving (Eq, Show)
type DatumName = String

type RecordDescr = (DatumName, DatumType, DatumRepeatCount)

type DatumRepeatCount = Int
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

datumTypeIntMap :: [(DatumType, Int)]
datumTypeIntMap = [(DInvalid, 0),(DChar, 1),(DShort,2),(DInt,3)
                  ,(DFloat,4),(DDouble,5),(DFunc,6),(DFFunc,7)
                  ,(DULong,8),(DUnknown,-1)]
datumTypeToIntegral :: DatumType -> Int
datumTypeToIntegral d = maybe (-1) id (lookup d datumTypeIntMap)
datumTypeFromIntegral :: Int -> DatumType
datumTypeFromIntegral i =
  maybe DUnknown id (lookup i (map(\(a,b)->(b,a)) datumTypeIntMap))

data FileInfo = FileInfo { hProgram     :: String
                         , hVersion     :: String
                         , hArgv        :: [String]
                         , hDate        :: String
                         , hDir         :: String
                         , hHostname    :: String
                         , hArch        :: String
                         , hUser        :: String
                         , hFileType    :: FileType
                         , hExtractT    :: String
                         , hProbe       :: Int
                         , hNTrodes     :: Int
                         , hNTrodeChans :: Int
                         , hRecMode     :: RecordMode
                         , hRecordDescr :: [RecordDescr]
                         , hChanDescrs  :: [ChanDescr]
                         } deriving (Eq, Show)


data ChanDescr = ChanDescr AmpGain AdGain FilterCode Threshold ColorCode
                 deriving (Eq, Show)
type AmpGain = Double
type AdGain  = Double
type FilterCode = Int
type Threshold = Double
type ColorCode = Int