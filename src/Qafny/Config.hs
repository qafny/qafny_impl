{-# LANGUAGE
    StrictData
  , TypeFamilies
  #-}

module Qafny.Config where

import           Qafny.TTG
import           Qafny.Utils.Common

data Mode
  = Verify
  | Format

data ConfigsT t = Configs
  { stdlibPath :: T t String
  , mode       :: T t Mode
  , filePath   :: T t FilePath
  }

-- | Full configurations
type Configs = ConfigsT Identity

-- | Partial configurations
type ConfigsP = ConfigsT Maybe

defaultConfigsP :: ConfigsP
defaultConfigsP = Configs Nothing Nothing Nothing


projConfigs ::
  forall m . Monad m => (forall a . String -> m a) -> ConfigsP -> m Configs
projConfigs pError Configs{stdlibPath, mode, filePath} =
  Configs
  <$> handle "stdlibPath" stdlibPath
  <*> handle "mode"       mode
  <*> handle "filePath"   filePath
  where
    handle :: forall b . String -> Maybe b -> m b
    handle name = maybe
      (pError ("Config field '"++name++"' has not been constructed!"))
      pure
