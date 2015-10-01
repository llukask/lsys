module Main where

import qualified Data.Map.Strict as Map
import Data.Char
import Control.Applicative


data LSystem symbol = LSystem (Map.Map symbol [symbol])
  deriving (Show)

getRules :: LSystem sym -> Map.Map sym [sym]
getRules (LSystem rules) = rules

nextLSys :: Ord sym => LSystem sym -> [sym] -> [sym]
nextLSys (LSystem rules) = concatMap getSym
  where getSym x = if Map.member x rules
                    then (Map.!) rules x
                    else [x]

algaeSys = LSystem (Map.fromList [('A', "AB"), ('B', "A")])

nextAlgae :: String -> String
nextAlgae = nextLSys algaeSys

pythagorasTreeSys = LSystem (Map.fromList [ ('1', "11")
                                          , ('0', "1[0]0")
                                          , ('[', "[")
                                          , (']', "]")])

x = "█"

nextPythagoras :: String -> String
nextPythagoras = nextLSys pythagorasTreeSys

candorDustSys = LSystem (Map.fromList [('█', "█ █"), (' ', "   ")])

nextCandor :: String -> String
nextCandor = nextLSys candorDustSys

data PlantDirections = X
                     | Forward
                     | TurnL
                     | TurnR
                     | Save
                     | Restore
              deriving (Ord, Eq)

charToPlantDirection :: Char -> PlantDirections
charToPlantDirection 'X' = X
charToPlantDirection 'F' = Forward
charToPlantDirection '-' = TurnL
charToPlantDirection '+' = TurnR
charToPlantDirection '[' = Save
charToPlantDirection ']' = Restore

strToPlantDirections :: String -> [PlantDirections]
strToPlantDirections = map charToPlantDirection

plantDirectionToChar :: PlantDirections -> Char
plantDirectionToChar X        = 'X'
plantDirectionToChar Forward  = 'F'
plantDirectionToChar TurnL    = '-'
plantDirectionToChar TurnR    = '+'
plantDirectionToChar Save     = '['
plantDirectionToChar Restore  = ']'

plantDirectionsToStr :: [PlantDirections] -> String
plantDirectionsToStr = map plantDirectionToChar

instance Show PlantDirections where
  show = pure . plantDirectionToChar

plantSys = LSystem (Map.fromList [ (X, strToPlantDirections "F-[[X]+X]+F[+FX]-X")
                                 , (Forward, strToPlantDirections "FF") ])

nextPlantSys = nextLSys plantSys

main :: IO ()
main = putStrLn "hello world"
