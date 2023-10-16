{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Render
	( Renderable(renderText)
	, putText
	) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.String (fromString)

-- Things that can be printed for the user
class Renderable x where
	renderText :: x -> Text

instance Renderable Integer where
	renderText = fromString . show

instance Renderable Text where
	renderText = id

instance Renderable String where
	renderText = fromString

instance Renderable Bool where
	renderText = fromString . show

putText :: Renderable x => x -> IO ()
putText = TIO.putStr . renderText
