{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (_Object)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Forward
import GHC.Generics (Generic)
import qualified Slick

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
  SiteMeta
    { siteAuthor = "Me",
      baseUrl = "https://example.com",
      siteTitle = "My Slick Site",
      twitterHandle = Just "myslickhandle",
      githubUser = Just "myslickgithubuser"
    }

outputFolder :: FilePath
outputFolder = "docs/"

-- Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ KM.union obj siteMetaObj
  where
    Object siteMetaObj = Aeson.toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta = SiteMeta
  { siteAuthor :: String,
    baseUrl :: String, -- e.g. https://example.ca
    siteTitle :: String,
    twitterHandle :: Maybe String, -- Without @
    githubUser :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo = IndexInfo
  { posts :: [Post]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type Tag = String

-- | Data for a blog post
data Post = Post
  { title :: String,
    author :: String,
    content :: String,
    url :: String,
    date :: String,
    tags :: [Tag],
    description :: String,
    image :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data AtomData = AtomData
  { title :: String,
    domain :: String,
    author :: String,
    posts :: [Post],
    currentTime :: String,
    atomUrl :: String
  }
  deriving (Generic, ToJSON, Eq, Ord, Show)

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> Action ()
buildIndex posts' = do
  indexT <- Slick.compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {posts = posts'}
      indexHTML = Text.unpack (Slick.substitute indexT (withSiteMeta (Aeson.toJSON indexInfo)))
  writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
  forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- Slick.markdownToHTML . Text.pack $ postContent
  let postUrl = Text.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta . withPostUrl $ postData
  template <- Slick.compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> Text.unpack postUrl) (Text.unpack (Slick.substitute template fullPostData))
  Slick.convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
  where
    parsedTime =
      Time.parseTimeOrError True Time.defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:SZ"

-- format' :: Format TimeOfDay
-- format' = ISO8601.hourMinuteFormat ExtendedFormat
-- toIsoDate = Time.formatTime Time.defaultTimeLocale _ -- (ISO8601.formatShow format')

toIsoDate :: UTCTime -> String
toIsoDate = Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat rfc3339)

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  now <- liftIO Time.getCurrentTime
  let atomData =
        AtomData
          { title = siteTitle siteMeta,
            domain = baseUrl siteMeta,
            author = siteAuthor siteMeta,
            posts = mkAtomPost <$> posts,
            currentTime = toIsoDate now,
            atomUrl = "/atom.xml"
          }
  atomTempl <- Slick.compileTemplate' "site/templates/atom.xml"
  writeFile' (outputFolder </> "atom.xml") (Text.unpack (Slick.substitute atomTempl (Aeson.toJSON atomData)))
  where
    mkAtomPost :: Post -> Post
    mkAtomPost p = p {date = formatDate $ date p}

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  buildIndex allPosts
  buildFeed allPosts
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = shakeOptions {shakeVerbosity = Chatty, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules
