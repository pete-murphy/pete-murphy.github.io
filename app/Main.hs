{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow ((>>>))
import Control.Lens (At (..), (?~), (^.), (^..), (^?), _Unwrapped')
import Control.Monad qualified as Monad
import Data.Aeson (FromJSON, ToJSON, Value (..), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (_Integer, _Object, _String)
import Data.Aeson.Lens qualified as Aeson
import Data.Function ((&))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Ord (Down (Down))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lens qualified as Text
import Data.Time (UTCTime)
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import Deriving.Aeson.Stock (CustomJSON (..), PrefixedSnake)
import Development.Shake (Action, ShakeOptions (..), Verbosity (..))
import Development.Shake qualified as Shake
import Development.Shake.Classes (Binary)
import Development.Shake.FilePath ((-<.>), (</>))
import Development.Shake.FilePath qualified as Shake.FilePath
import Development.Shake.Forward qualified as Shake.Forward
import GHC.Generics (Generic)
import Multicodeblock qualified
import Slick qualified
import Slick.Pandoc qualified
import Title qualified

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
  SiteMeta
    { siteAuthor = "Pete",
      baseURL = "https://ptrfrncsmrph.github.io",
      siteTitle = "WIP",
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
    via PrefixedSnake "index" IndexInfo

data Tag = Tag
  { tagName :: String,
    tagPosts :: [Post],
    tagURL :: String
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Binary)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "tag" Tag

-- | Data for a blog post
data Post = Post
  { postTitle :: String,
    -- postDisplayTitle :: String,
    -- postHTMLTitle :: String,
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
    postReadingTime :: Integer,
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
        -- TODO: The above are all implicitly failing with `mempty` if the key
        -- is missing. Doing the same here for now (so wrapping in `Sum`) but
        -- should probably handle parse errors.
        postReadingTime = getSum (value ^. Aeson.key "readingTime" . _Integer . _Unwrapped')

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
        "readingTime" .= postReadingTime,
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
    via PrefixedSnake "atom" AtomData

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
  -- pre-process markdown to replace `<Multicodeblock>` tags with
  -- `<multicodeblock-tab>` and `<multicodeblock-panel>` custom elements
  -- TODO: Could this be a PandocReader?
  (title, sanitizedTitle, rest) <- Title.parse postContent
  postContentWithCodeBlocks <- Multicodeblock.parse rest
  let wordCount = length (words rest)
      readingTime = wordCount `div` 200
      withReadingTime = _Object . at "readingTime" ?~ Number (fromIntegral readingTime)

  -- TODO: Allow HTML title vs text title
  let withTitle =
        _Object . at "title" ?~ String (Text.pack title)
          >>> _Object . at "displayTitle" ?~ String (Text.pack sanitizedTitle)

  let postURL = Text.pack (Shake.FilePath.dropDirectory1 (srcPath -<.> "html"))
      withPostURL = _Object . at "url" ?~ String postURL

  postData <- Slick.Pandoc.markdownToHTML (Text.pack postContentWithCodeBlocks)

  -- Add additional metadata we've been able to compute
  let fullPostData =
        Aeson.toJSON postData
          & withPostURL
          & withSiteMeta
          & withTitle
          & withReadingTime

  template <- Slick.compileTemplate' "site/templates/post.html"
  Shake.writeFile' (outputFolder </> Text.unpack postURL) (Text.unpack (Slick.substitute template fullPostData))
  Slick.convert fullPostData

buildTags :: [Tag] -> Action ()
buildTags tags =
  Monad.void do
    Shake.forP tags writeTag

writeTag :: Tag -> Action ()
writeTag tag@Tag {tagURL} = do
  tagTempl <- Slick.compileTemplate' "site/templates/tag.html"
  Shake.writeFile' (outputFolder <> tagURL -<.> "html") (Text.unpack (Slick.substitute tagTempl (Aeson.toJSON tag)))

getTags :: [Post] -> Action [Tag]
getTags posts = do
  let tagToPostsSet = Map.unionsWith mappend (toMap <$> posts)
      tagToPostsList = fmap Set.toList tagToPostsSet
      tagObjects =
        Map.foldMapWithKey
          (\tagName ps -> [Tag {tagName, tagPosts = sortByDate ps, tagURL = "/tag/" <> tagName}])
          tagToPostsList
  pure tagObjects
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
