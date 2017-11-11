{-# LANGUAGE TemplateHaskell #-}
module TestTemplate where

import Template

$(sequence [composeN 3])
$(sequence (map composeMap' [1..40]))
