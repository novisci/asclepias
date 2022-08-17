{-|
Functions for initializing an asclepias project.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Hasklepias.Initialize where 

import Text.Mustache 
import System.Directory
import System.FilePath

import Development.GitRev
import Data.Bifunctor
import Data.Aeson
import qualified Data.Set as Set
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception

import qualified Data.ByteString
import Data.FileEmbed

{-|
-}
data ProjectInitSettings = MkProjectInitSettings {
    prjId :: Text
  , prjShortName :: Text
  , prjLongName :: Text
  , prjSponsor :: Text
  , asclepiasGitUrl :: Text
  , asclepiasGitTag :: Text
  , edmGitUrl :: Text
  , edmGitTag :: Text
} deriving (Eq, Show, Generic)

instance ToJSON ProjectInitSettings

{-|
-}
defaultValues :: ProjectInitSettings
defaultValues = MkProjectInitSettings {
    prjId = "P0000"
  , prjShortName = "Test Project"
  , prjLongName = "Test Project"
  , prjSponsor = "NoviSci"
  , asclepiasGitUrl = "https://gitlab+deploy-token-ns-projects:glpat-LCmSwKeZSLheZCTVMEkA@gitlab.novisci.com/nsStat/asclepias.git"
  , asclepiasGitTag = pack $(gitHash)
  , edmGitUrl = "https://gitlab+deploy-token-ns-projects:glpat-7Z4w2JGrm2692Bshcqhd@gitlab.novisci.com/nsStat/event-data-model.git"
  , edmGitTag = "b194e74bebcb32ffdcbebc5bb00c07f680b7bada"
}

{-|
-}
xx :: MonadIO m => m [(FilePath, Text)]
xx = traverse (traverse f)
  -- $(embedDir "hasklepias-main/project-templates/default")
  $(embedDir "project-templates/default")
  where 
    f x = case decodeUtf8' x of
      Left e -> liftIO $ throwIO e
      Right v -> pure v

defaultTemplate :: MonadIO m => m ProjectTemplate
defaultTemplate = compileProjectTemplate =<< traverse (traverse f)
  -- $(embedDir "hasklepias-main/project-templates/default")
  $(embedDir "project-templates/default")
  where 
    f x = case decodeUtf8' x of
      Left e -> liftIO $ throwIO e
      Right v -> pure v

type FileTemplate = (Template, Template)
type ProjectTemplate = [FileTemplate]

{-|
-}
compileProjectTemplateFile :: MonadIO m => (FilePath, Text) -> m FileTemplate
compileProjectTemplateFile x = 
  g $ bimap 
    (compileMustacheText "Filename" . pack) 
    (compileMustacheText "Contents")
    x
  where 
    g v = case v of
      (Right y, Right z) -> pure (y, z)
      (Left e, _)  -> liftIO $ throwIO $ MustacheParserException e
      (_ , Left e) -> liftIO $ throwIO $ MustacheParserException e

{-|
-}
compileProjectTemplate :: MonadIO m => [(FilePath, Text)] -> m ProjectTemplate
compileProjectTemplate = traverse compileProjectTemplateFile

{-|
-}
renderProjectTemplate :: ProjectTemplate -> Value -> [(FilePath, TL.Text)]
renderProjectTemplate t v = 
   fmap (bimap (dropExtension . unpack . TL.toStrict . f) f) t
   where f = flip renderMustache v

{-|
-}

f :: IO [(FilePath, TL.Text)]
f = liftM2 renderProjectTemplate defaultTemplate (pure $ toJSON defaultValues)


main :: IO ()
main = do
  z <- liftM2 renderProjectTemplate defaultTemplate (pure $ toJSON defaultValues)
  let z2 = fmap (first (\x -> "testDir" </> x)) z

  let neededDirs = Set.toList $ Set.fromList $ fmap (takeDirectory . fst) z2

  mapM createDirectory neededDirs
  -- print neededDirs
  mapM (uncurry TL.writeFile) z2

  pure ()
  
