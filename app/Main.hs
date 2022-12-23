{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens (At (..), (?~), (^.), (^..), (^?))
import qualified Control.Monad as Monad
import Data.Aeson (FromJSON, ToJSON, Value (..), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Lens (_Object, _String)
import qualified Data.Aeson.Lens as Aeson
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down (Down))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lens as Text
import Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as ISO8601
import Deriving.Aeson (FieldLabelModifier, StripPrefix)
import Deriving.Aeson.Stock (CustomJSON (..))
import Development.Shake
  ( Action,
    ShakeOptions (..),
    Verbosity (..),
  )
import qualified Development.Shake as Shake
import Development.Shake.Classes (Binary)
import Development.Shake.FilePath ((-<.>), (</>))
import qualified Development.Shake.FilePath as Shake.FilePath
import qualified Development.Shake.Forward as Shake.Forward
import GHC.Generics (Generic)
import qualified Slick

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
  SiteMeta
    { siteAuthor = "Pete",
      baseURL = "https://ptrfrncsmrph.github.io",
      siteTitle = "TIL",
      twitterHandle = Nothing,
      githubUser = Just "ptrfrncsmrph"
    }

outputFolder :: FilePath
outputFolder = "docs/"

-- Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object (KeyMap.union obj siteMetaObj)
  where
    Object siteMetaObj = Aeson.toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta = SiteMeta
  { siteAuthor :: String,
    baseURL :: String, -- e.g. https://example.ca
    siteTitle :: String,
    twitterHandle :: Maybe String, -- Without @
    githubUser :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo = IndexInfo
  { indexPosts :: [Post],
    indexTags :: [Tag]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "index"]] IndexInfo

data Tag = Tag
  { tagName :: String,
    tagPosts :: [Post],
    tagURL :: String
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Binary)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "tag"]] Tag

-- | Data for a blog post
data Post = Post
  { postTitle :: String,
    postAuthor :: String,
    postContent :: String,
    postURL :: String,
    postImage :: Maybe String,
    postTags :: [String],
    postNextPostURL :: Maybe String,
    postPrevPostURL :: Maybe String,
    postISODate :: String,
    postDate :: String,
    postSrcPath :: String,
    postDescription :: String,
    postSlug :: String
  }
  deriving (Generic, Eq, Ord, Show, Binary)

instance FromJSON Post where
  parseJSON value = do
    let postTitle = value ^. Aeson.key "title" . _String . Text.unpacked
        postAuthor = value ^. Aeson.key "author" . _String . Text.unpacked
        postDate = value ^. Aeson.key "date" . _String . Text.unpacked
        postISODate = formatDate postDate
        postContent = value ^. Aeson.key "content" . _String . Text.unpacked
        postURL = value ^. Aeson.key "url" . _String . Text.unpacked
        postTags = value ^.. Aeson.key "tags" . Aeson.values . _String . Text.unpacked
        postNextPostURL = Nothing
        postPrevPostURL = Nothing
        postSrcPath = value ^. Aeson.key "srcPath" . _String . Text.unpacked
        postImage = value ^? Aeson.key "image" . _String . Text.unpacked
        postDescription = value ^. Aeson.key "description" . _String . Text.unpacked
        postSlug = value ^. Aeson.key "slug" . _String . Text.unpacked
    pure Post {..}

instance ToJSON Post where
  toJSON Post {..} =
    Aeson.object
      [ "title" .= postTitle,
        "author" .= postAuthor,
        "content" .= postContent,
        "url" .= postURL,
        "image" .= postImage,
        "tags" .= postTags,
        "nextPostURL" .= postNextPostURL,
        "prevPostURL" .= postPrevPostURL,
        "isoDate" .= postISODate,
        "date" .= postDate,
        "srcPath" .= postSrcPath,
        "description" .= postDescription,
        "slug" .= postSlug
      ]

data AtomData = AtomData
  { atomTitle :: String,
    atomDomain :: String,
    atomAuthor :: String,
    atomPosts :: [Post],
    atomCurrentTime :: String,
    atomAtomURL :: String
  }
  deriving (Generic, Eq, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "atom"]] AtomData

-- | given a list of posts this will build a table of contents
buildIndex :: [Post] -> [Tag] -> Action ()
buildIndex allPosts allTags = do
  indexT <- Slick.compileTemplate' "site/templates/index.html"
  let indexInfo = IndexInfo {indexPosts = allPosts, indexTags = allTags}
      indexHTML = Text.unpack (Slick.substitute indexT (withSiteMeta (Aeson.toJSON indexInfo)))
  Shake.writeFile' (outputFolder </> "index.html") indexHTML

-- | Find and build all posts
buildPosts :: Action [Post]
buildPosts = do
  pPaths <- Shake.getDirectoryFiles "." ["site/posts//*.md"]
  Shake.forP pPaths buildPost

-- | Load a post, process metadata, write it to output, then return the post object
-- Detects changes to either post content or template
buildPost :: FilePath -> Action Post
buildPost srcPath = Shake.Forward.cacheAction ("build" :: Text, srcPath) do
  Shake.liftIO (putStrLn ("Rebuilding post: " <> srcPath))
  postContent <- Shake.readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- Slick.markdownToHTML (Text.pack postContent)
  let postURL = Text.pack (Shake.FilePath.dropDirectory1 (srcPath -<.> "html"))
      withPostURL = _Object . at "url" ?~ String postURL
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta (withPostURL postData)
  template <- Slick.compileTemplate' "site/templates/post.html"
  Shake.writeFile' (outputFolder </> Text.unpack postURL) (Text.unpack (Slick.substitute template fullPostData))
  Slick.convert fullPostData

buildTags :: [Tag] -> Action ()
buildTags tags =
  Monad.void do
    Shake.forP tags writeTag

writeTag :: Tag -> Action ()
writeTag t@Tag {tagURL} = do
  tagTempl <- Slick.compileTemplate' "site/templates/tag.html"
  Shake.writeFile' (outputFolder <> tagURL -<.> "html") (Text.unpack (Slick.substitute tagTempl (Aeson.toJSON t)))

getTags :: [Post] -> Action [Tag]
getTags posts = do
  let tagToPostsSet = Map.unionsWith mappend (toMap <$> posts)
      tagToPostsList = fmap Set.toList tagToPostsSet
      tagObjects =
        Map.foldMapWithKey
          (\tagName ps -> [Tag {tagName, tagPosts = sortByDate ps, tagURL = "/tag/" <> tagName}])
          tagToPostsList
  return tagObjects
  where
    toMap :: Post -> Map String (Set Post)
    toMap p@Post {postTags} = Map.unionsWith mappend (embed p <$> postTags)
    embed :: Post -> String -> Map String (Set Post)
    embed post tag = Map.singleton tag (Set.singleton post)

sortByDate :: [Post] -> [Post]
sortByDate = List.sortOn (Down . postISODate)

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- Shake.getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
  Monad.void do
    Shake.forP filepaths \filepath ->
      Shake.copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

formatDate :: String -> String
formatDate humanDate = toISODate parsedTime
  where
    parsedTime =
      Time.parseTimeOrError True Time.defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

toISODate :: UTCTime -> String
toISODate = ISO8601.formatShow ISO8601.iso8601Format

buildFeed :: [Post] -> Action ()
buildFeed posts = do
  now <- Shake.liftIO Time.getCurrentTime
  let atomData =
        AtomData
          { atomTitle = siteTitle siteMeta,
            atomDomain = baseURL siteMeta,
            atomAuthor = siteAuthor siteMeta,
            atomPosts = mkAtomPost <$> posts,
            atomCurrentTime = toISODate now,
            atomAtomURL = "/atom.xml"
          }
  atomTempl <- Slick.compileTemplate' "site/templates/atom.xml"
  Shake.writeFile' (outputFolder </> "atom.xml") (Text.unpack (Slick.substitute atomTempl (Aeson.toJSON atomData)))
  where
    mkAtomPost :: Post -> Post
    mkAtomPost p = p {postDate = formatDate (postDate p)}

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  allPosts <- buildPosts
  allTags <- getTags allPosts
  buildTags allTags
  buildIndex allPosts allTags
  buildFeed allPosts
  copyStaticFiles

main :: IO ()
main = do
  let shOpts = Shake.shakeOptions {shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
  Shake.Forward.shakeArgsForward shOpts buildRules
