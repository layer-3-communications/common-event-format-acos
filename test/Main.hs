{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}

import Sample

import Cef (Event(Event))
import Control.Monad (when)
import Cef.Extension.Acos (Field(SourceIp,DestinationPort),interpret)

import qualified Cef
import qualified Net.IP as IP

main :: IO ()
main = do
  putStrLn "Test A"
  case Cef.decode sampleA10_a of
    Nothing -> fail "Failed to parse"
    Just Event{extension} -> do
      let xs = interpret extension
      when (notElem (SourceIp (IP.ipv4 192 0 2 105)) xs) $ do
        print xs
        fail "Incorrect src"
      when (notElem (DestinationPort 1001) xs) $ do
        print xs
        fail "Incorrect dpt"
  putStrLn "Test A Succeeded"
  putStrLn "Complete"

