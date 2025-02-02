{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Fixed (div')
import Data.Function (on)
import Data.List (intersperse, isPrefixOf, sortBy)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Network.HTTP.Types
import qualified Network.URI.Encode as URI
import Network.Wai
import Network.Wai.Handler.Warp
import Options.Applicative
import System.Directory
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  config <- customExecParser (prefs showHelpOnEmpty) configP
  setCurrentDirectory config.root

  let warpSettings = setHost config.listenAddress $ setPort config.listenPort defaultSettings
  currentRoot <- getCurrentDirectory
  putStrLn $ "Serving " <> currentRoot <> " on " <> show config.listenAddress <> ":" <> show config.listenPort
  runSettings warpSettings $ app config

app :: Config -> Application
app config req respond = do
  let targetPath' = foldr ((</>) . T.unpack . URI.decodeText) "" req.pathInfo
      targetPath = if null targetPath' then "." else targetPath'
      respond404 = ($ responseLBS status404 [] "404 Not Found")
      respond403 = ($ responseLBS status403 [] "403 Forbidden")

  now <- getCurrentTime
  currentRoot <- getCurrentDirectory
  isRequestOnRoot <- ("." ==) <$> makeRelativeToCurrentDirectory targetPath
  absoluteRequestedTarget <- makeAbsolute targetPath

  if not $ currentRoot `isPrefixOf` absoluteRequestedTarget
    then respond403 respond
    else do
      exists <- doesPathExist targetPath
      if not exists
        then respond404 respond
        else do
          readable <- readable <$> getPermissions targetPath
          if not readable
            then respond403 respond
            else do
              isFile <- doesFileExist targetPath
              if isFile
                then respond $ responseFile status200 [] targetPath Nothing
                else do
                  paths <- getDirectoryContents targetPath
                  details <-
                    forM paths $ \p -> do
                      fp <- makeRelativeToCurrentDirectory (targetPath </> p)
                      let up = config.baseUrl <</>> T.unpack (T.intercalate "/" $ map URI.encodeText $ T.splitOn "/" $ T.pack fp)
                      PathInfo p up
                        <$> (utcToZonedTime <$> (getModificationTime fp >>= getTimeZone) <*> getModificationTime fp)
                        <*> getFileSize fp
                        <*> doesFileExist fp

                  respond $ responseLBS status200 [] $ listBody config now req isRequestOnRoot targetPath details

-- * List directory
listBody :: Config -> UTCTime -> Request -> Bool -> FilePath -> [PathInfo] -> BL.ByteString
listBody config now req isRequestOnRoot targetPath details =
  BL8.unlines
    [ "<html>",
      "  <head>",
      "    <title>" <> title <> "</title>",
      "    <meta charset=\"UTF-8\" />",
      "  </head>",
      "  <style>",
      "  table {",
      "    border-collapse: collapse;",
      "    width: 100%;",
      "  }",
      "  table td:not(:first-child),",
      "  table th:not(:first-child) {",
      "      text-align: center;",
      "      vertical-align: middle;",
      "  }",
      "  table tbody tr:nth-child(odd) {",
      "    background-color: #f2f2f2;",
      "  }",
      "  table tbody tr:hover {",
      "    background-color: #e9e9e9;",
      "  }",
      "  table thead .selected {",
      "      font-style: italic;",
      "  }",
      "  table thead a {",
      "      text-decoration: none;",
      "      color: #000000;",
      "  }",
      "  table thead a:visited {",
      "      color: #000000;",
      "  }",
      "  table tbody a {",
      "      text-decoration: none;",
      "      color: #007bff;",
      "  }",
      "  table tbody a:visited {",
      "      color: #5996d2;",
      "  }",
      "  </style>",
      "  <body>",
      "    <h1>" <> title <> "</h1>",
      "    <hr />",
      "    <table>",
      "      <thead>",
      "        <tr>" <>  mconcat tableHeaders <> "</tr>",
      "      </thead>",
      "      <tbody>",
      BL8.unlines
        [ "        <tr><td>" <> mconcat (intersperse "</td><td>" $ lineParts line) <> "</td></tr>"
          | line <- sortBy sorting $ filter (isValidPath . (.shortPath)) details
        ],
      "      </tbody>",
      "    </table>",
      "    <hr />",
      "  </body>",
      "</html>"
    ]
    where toBSL = TE.encodeUtf8 . T.pack
          toBS = BL.fromStrict . toBSL
          isValidPath p = p /= "." && (not isRequestOnRoot || p /= "..")
          lineParts p =
            if p.isPlainFile
              then
                [ "<a href=\"" <> toBS p.urlPath <> "\">" <> toBS p.shortPath <> "</a>",
                  let absolute = formatTime defaultTimeLocale "%F %R" p.mtime
                      diffTime = nominalDiffTimeToSeconds $ diffUTCTime now $ zonedTimeToUTC p.mtime
                      oneMinute = 60
                      ago x = "<i><abbr title=\"" <> absolute <> "\">" <> x <> " ago</abbr></i>"
                   in toBS $
                        case checkQueryParam config.dateFormat "dateFormat" dateFormatP of
                          DateAbsolute -> absolute
                          DateRelative
                            | diffTime < oneMinute ->
                                ago "less than one minute"
                            | diffTime < 60 * oneMinute ->
                                ago $ show @Integer (diffTime `div'` oneMinute) <> " minute(s)"
                            | diffTime < 24 * 60 * oneMinute ->
                                ago $ show @Integer (diffTime `div'` (60 * oneMinute)) <> " hour(s)"
                            | otherwise ->
                                absolute,
                  toBS $ uncurry (<>) $ formatSize $ fromInteger p.size
                ]
              else
                [ "<a href=\"" <> toBS p.urlPath <> "\">" <> toBS p.shortPath <> "/</a>",
                  "-",
                  "-"
                ]
          sortingOrderV = checkQueryParam config.sortingOrder "sortingOrder" sortingOrderP
          sortingOrder' :: (Ord a) => a -> a -> Ordering
          sortingOrder' =
            case sortingOrderV of
              SortingAsc -> compare
              SortingDesc -> flip compare
          sortingFieldV = checkQueryParam config.sortingField "sortingField" sortingFieldP
          sorting :: PathInfo -> PathInfo -> Ordering
          sorting =
            case sortingFieldV of
              SortingByName -> sortingOrder' `on` (.shortPath)
              SortingByMtime -> sortingOrder' `on` zonedTimeToUTC . (.mtime)
              SortingBySize -> sortingOrder' `on` (.size)
          tableHeaders =
            let h n x =
                  if sortingFieldV == x
                  then "<th class=\"selected\"><a href=\"" <> link x (revertSortingOrder sortingOrderV) <> "\">" <> n <> "</a></th>"
                  else "<th><a href=\"" <> link x SortingAsc <> "\">" <> n <> "</a></th>"
                link x o =
                  toBS (config.baseUrl <</>> targetPath)
                    <> BL.fromStrict (renderQuery True [("sortingOrder", Just $ toBSL $ show o), ("sortingField", Just $ toBSL $ show x)])
            in [
              h "Name" SortingByName,
              h "Last modification" SortingByMtime,
              h "Size" SortingBySize
            ]
          checkQueryParam :: a -> B.ByteString -> (B.ByteString -> Either e a) -> a
          checkQueryParam d n p =
            fromMaybe d $ join (lookup n req.queryString) >>= either (const Nothing) Just . p
          title = "Index of " <> toBS (config.baseUrl <</>> if not isRequestOnRoot then targetPath else "/")

          formatSize :: Double -> (String, String)
          formatSize starting = (cropped, unit)
            where
              (unit, ref) = last $ zip ["B", "KiB", "MiB", "GiB", "TiB", "PiB"] $ takeWhile (>= 1) $ iterate (/ 1024.0) starting
              cropped
                | ref > -100 = take 4 $ show ref
                | otherwise = takeWhile (/= '.') $ show ref

data PathInfo = PathInfo
  { shortPath :: FilePath,
    urlPath :: Url,
    mtime :: ZonedTime,
    size :: Integer,
    isPlainFile :: Bool
  }

-- * Config
data Config = Config
  { listenAddress :: HostPreference,
    listenPort :: Int,
    baseUrl :: Url,
    root :: FilePath,
    sortingOrder :: SortingOrder,
    sortingField :: SortingField,
    dateFormat :: DateFormat
  }

data SortingOrder = SortingAsc | SortingDesc

instance Show SortingOrder where
  show =
    \case
      SortingAsc -> "asc"
      SortingDesc -> "desc"

revertSortingOrder :: SortingOrder -> SortingOrder
revertSortingOrder =
  \case
    SortingAsc -> SortingDesc
    SortingDesc -> SortingAsc

data SortingField = SortingByName | SortingByMtime | SortingBySize deriving stock (Eq)

instance Show SortingField where
  show =
    \case
      SortingByName -> "name"
      SortingByMtime -> "mtime"
      SortingBySize -> "size"

data DateFormat = DateAbsolute | DateRelative

instance Show DateFormat where
  show =
    \case
      DateAbsolute -> "absolute"
      DateRelative -> "relative"

configP :: ParserInfo Config
configP = info (parser <**> helper) (fullDesc <> header "Simple HTTP Haskell Server")
  where
    parser =
      Config
        <$> option str (long "host" <> metavar "ADDRESS" <> value "127.0.0.1" <> help "Listening host" <> showDefault)
        <*> option auto (long "port" <>  metavar "PORT" <> value 8080 <> help "Listening port")
        <*> option str (long "base-url" <> metavar "URL" <> value "/" <> help "Base url" <> showDefault)
        <*> option str (long "root" <> metavar "PATH" <> value "." <> help "Root directory" <> showDefault)
        <*> option (eitherReader sortingOrderP) (long "sorting-order" <> metavar "ORDER" <> value SortingAsc <> help "Sorting order" <> showDefault)
        <*> option (eitherReader sortingFieldP) (long "sorting-field" <> metavar "FIELD" <> value SortingByName <> help "Sorting field" <> showDefault)
        <*> option (eitherReader dateFormatP) (long "date-format" <> metavar "FORMAT" <> value DateAbsolute <> help "Date format" <> showDefault)

sortingOrderP :: (IsString a, Eq a) => a -> Either String SortingOrder
sortingOrderP =
  \case
    "asc" -> Right SortingAsc
    "desc" -> Right SortingDesc
    _ -> Left "should be 'asc' or 'desc'"

sortingFieldP :: (IsString a, Eq a) => a -> Either String SortingField
sortingFieldP =
  \case
    "name" -> Right SortingByName
    "mtime" -> Right SortingByMtime
    "size" -> Right SortingBySize
    _ -> Left "should be 'name' or 'mtime' or 'size"

dateFormatP :: (IsString a, Eq a) => a -> Either String DateFormat
dateFormatP =
  \case
    "absolute" -> Right DateAbsolute
    "relative" -> Right DateRelative
    _ -> Left "should be 'absolute' or 'relative'"

-- * Utils
type Url = String

infix 7 <</>>

(<</>>) :: Url -> Url -> Url
(<</>>) b n =
  case (b, n) of
    ([], '/':_) -> n
    ([], _) -> "/" <> n
    ("/", '/':_) -> n
    ("/", _) -> "/" <> n
    (x:xs, _) -> x : (xs <</>> n)
