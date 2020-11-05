{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}

module Cef.Acos.Patterns where

import Data.Bytes.Patterns (makeBytesPatterns)

makeBytesPatterns
  [ "dpt"
  , "spt"
  , "dst"
  , "src"
  , "cnt"
  , "act"
  , "msg"
  , "cs1"
  , "cs1Label"
  , "cs2"
  , "cs2Label"
  , "cs3"
  , "cs3Label"
  ]
