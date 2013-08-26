module FileHeader where

import Text.Parsec

data FileType = Binary | Ascii deriving (Eq, Show)

data RecordDescr = RecordDescr RecordDatum DatumType DatumCount
                   deriving (Eq, Show)

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
               deriving (Eq, Show)

datumIntMap = [(DInvalid, 0),(DChar, 1),(DShort,2),(DInt,3)
              ,(DFloat,4),(DDouble,5),(DFunc,6),(DFFunc,7)
              ,

data FileHeader = FileHeader { hProgram    :: String
                             , hVersion    :: String
                             , hArgv       :: [String]
                             , hDate       :: String
                             , hDir        :: String
                             , hHostname   :: String
                             , hArch       :: String
                             , hUser       :: String
                             , hFileType   :: FileType
                             , hExtractT   :: String
                             , hProbe      :: Int
                             , hRecordDesc :: [RecordDesc]