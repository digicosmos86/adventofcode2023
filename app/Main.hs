module Main where

import Text.Regex.Posix -- Importing the regex-posix module

main :: IO ()
main = do
  let inputString = "I have 3 apples"
  let pattern = "[0-9]+" -- A regex pattern to match one or more digits

  -- Check if the input string contains a substring that matches the pattern
  let containsDigits = inputString =~ pattern :: Bool

  if containsDigits
    then putStrLn "The string contains digits."
    else putStrLn "The string does not contain digits."