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
  ]

