module Test.Main where

import Prelude
import Effect (Effect)
import Control.Monad.Free (Free)
import Data.List (List(..), (:), singleton)
import Data.Either (Either(..))
import Data.Rational (Rational, (%), fromInt)
import Data.Unfoldable (replicate)
import Data.Abc.PSoM 
import Data.Abc.PSoM.DSL (toDSL)
import Data.Abc.PSoM.Translation (initialise, toPSoM) 
import Data.Abc.Parser (parse)
import Data.Midi.Instrument (InstrumentName(AcousticGrandPiano))
import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

assertPSoM :: String -> PSoMProgram -> Test
assertPSoM s target =
  let
    parseResult =
      parse s
  in
    case parseResult of
      Right tune ->
        let
          transformationState = initialise tune
        in
          Assert.equal target (toPSoM tune.body transformationState) 

      Left err ->
        failure ("parse failed: " <> (show err))

main :: Effect  Unit
main = runTest do
  suite "melody" do
    notesSuite
    repeatSuite
    -- dslSuite

notesSuite :: Free TestF Unit
notesSuite =
  suite "Translation of ABC to PSoM" do
    test "single note" do
       assertPSoM singleNote singleNoteProgram
    test "two notes" do
       assertPSoM twoNotes twoNoteProgram
    test "two notes separate bars" do
       assertPSoM twoBars twoNoteProgram
    test "note and rest" do
       assertPSoM noteAndRest noteAndRestProgram
    test "single chord" do
       assertPSoM singleChord singleChordProgram
    test "broken rhythm pair" do
       assertPSoM brokenRhythmPair brokenRhythmProgram
    test "tie" do
       assertPSoM tie singleNoteProgram
    test "triplet" do
       assertPSoM triplet tripletProgram
    -- this test shows that grace notes are ignored at the moment
    test "grace notes" do
       assertPSoM graceNotes gracedDProgram

repeatSuite :: Free TestF Unit
repeatSuite =
  suite "repeated sections and voltas" do       
    test "repeat once" do
       assertPSoM singleRepeat (repeatProgram 2)   
    test "two repeats" do
       assertPSoM doubleRepeat (repeatProgram 3) 
    test "simple volta" do
       assertPSoM simpleVolta simpleVoltaProgram
    test "complex volta 1" do
       assertPSoM complexVolta1 complexVoltaProgram
    test "complex volta 2" do
       assertPSoM complexVolta2 complexVoltaProgram
    test "complex volta 3" do
       assertPSoM complexVolta3 complexVoltaProgram3
    test "complex volta 3a" do
       assertPSoM complexVolta3a complexVoltaProgram3

-- use this just to dump a DSL string
dslSuite :: Free TestF Unit
dslSuite =
  suite "Translation of ABC to the full PSoM DSL" do
    test "grace notes" do
       Assert.equal "" (toDSL gracedDProgram AcousticGrandPiano)

headers :: String 
headers = 
  "X: 1\r\n"
  <> "T: headers\r\n"
  <> "M: 4/4\r\n"
  <> "L: 1/8\r\n"
  <> "K: D\r\n"

singleNote :: String 
singleNote = 
  headers <> "| D |\r\n"

singleNoteProgram :: PSoMProgram
singleNoteProgram =
  PSoMProgram { variables : singleton noteD
              , program : (0 : Nil)
              , tempo : fromInt 1
              }

twoNotes :: String 
twoNotes = 
  headers <> "| DE |\r\n"

twoBars :: String 
twoBars = 
  headers <> "| D | E |\r\n"

twoNoteProgram :: PSoMProgram
twoNoteProgram =
  PSoMProgram { variables : singleton (noteD <> noteE)
              , program : (0 : Nil)
              , tempo : fromInt 1
              }       

noteAndRest :: String 
noteAndRest = 
  headers <> "| Dz |\r\n"

noteAndRestProgram :: PSoMProgram
noteAndRestProgram =
  PSoMProgram { variables : singleton (noteD <> rest)
              , program : (0 : Nil)
              , tempo : fromInt 1
              }    


brokenRhythmPair :: String 
brokenRhythmPair = 
  headers <> "| D>E |\r\n"

brokenRhythmProgram :: PSoMProgram
brokenRhythmProgram =
  PSoMProgram { variables : singleton ( (noteMus (3 % 16) "D" ) <> (noteMus (1 % 16) "E" ))
              , program : (0 : Nil)
              , tempo : fromInt 1
              }           

tie :: String 
tie = 
  headers <> "| D/- | D/ |\r\n"

triplet :: String 
triplet = 
  headers <> "| (3DEF |\r\n"   

tripletProgram :: PSoMProgram
tripletProgram =
  PSoMProgram { variables : singleton tripletDEF
              , program : (0 : Nil)
              , tempo : fromInt 1
              }
 
singleChord :: String 
singleChord = 
  headers <> "| [DE] |\r\n"                  

graceNotes :: String 
graceNotes = 
  headers <> "| {FE}D |\r\n"            

singleRepeat :: String 
singleRepeat = 
  headers <> "|: DE :|\r\n"

doubleRepeat :: String 
doubleRepeat = 
  headers <> "|:: DE ::|\r\n"


repeatProgram :: Int -> PSoMProgram
repeatProgram count =
  let 
    program = 
      replicate count 0
  in
    PSoMProgram { variables : singleton (noteD <> noteE)
                , program 
                , tempo : fromInt 1
                }      

simpleVolta :: String 
simpleVolta= 
  headers <> " D |1 E :|2 F |\r\n"
                    
simpleVoltaProgram :: PSoMProgram
simpleVoltaProgram =
  PSoMProgram { variables : (noteD : noteE : noteFs : Nil)
               , program : (0 : 1 : 0 : 2 : Nil)
               , tempo : fromInt 1
               }      

complexVolta1 :: String 
complexVolta1 = 
  headers <> " D |1 E :|2 F :|3 E :|4 F |\r\n"

complexVolta2 :: String 
complexVolta2 = 
  headers <> " D |1,3 E :|2,4 F |\r\n"

complexVolta3 :: String 
complexVolta3 = 
  headers <> " D |1,2,3 E :|4 F |\r\n"

complexVolta3a :: String 
complexVolta3a = 
  headers <> " D |1-3 E :|4 F |\r\n"
                    
complexVoltaProgram :: PSoMProgram
complexVoltaProgram =
  PSoMProgram { variables : (noteD : noteE : noteFs : noteE : noteFs : Nil)
               , program : (0 : 1 : 0 : 2 : 0 : 3 : 0 : 4 : Nil)
               , tempo : fromInt 1
               }     

complexVoltaProgram3 :: PSoMProgram
complexVoltaProgram3 =
  PSoMProgram { variables : (noteD : noteE : noteE : noteE : noteFs : Nil)
               , program : (0 : 1 : 0 : 2 : 0 : 3 : 0 : 4 : Nil)
               , tempo : fromInt 1
               }      

noteD :: List PSMusic
noteD = 
  noteMus (1 % 8) "D"        

noteE :: List PSMusic
noteE =   
  noteMus (1 % 8) "E" 

noteFs :: List PSMusic
noteFs =   
  noteMus (1 % 8) "Fs" 

noteMus :: Rational -> String -> List PSMusic
noteMus duration pitchClass = 
  singleton (PSNOTE (note duration pitchClass))
  
  
  -- singleton (PSNOTE ( {graces: Nil, note: note duration pitchClass} ))

note :: Rational -> String -> PSNote 
note duration pitchClass = 
  PSNote { duration, octave: 4, pitchClass }

rest :: List PSMusic
rest =
  singleton (PSREST (PSRest { duration : 1 % 8 })) 

chordDE :: List PSMusic
chordDE = 
  singleton (PSCHORD ((note (1 % 8) "D") : (note (1 % 8) "E") : Nil )) 

singleChordProgram :: PSoMProgram
singleChordProgram =
  PSoMProgram { variables : singleton (chordDE)
              , program : (0 : Nil)
              , tempo : fromInt 1
              }

tripletDEF :: List PSMusic
tripletDEF = 
  singleton $ PSTUPLET $ PSRestOrNoteSequence
    { signature : (3 % 2)
    , notes : ( Right (note (1 % 8) "D")
              : Right (note (1 % 8) "E")
              : Right (note (1 % 8) "Fs")
              : Nil )
    }


gracedDProgram :: PSoMProgram
gracedDProgram =
  PSoMProgram { variables : singleton (gracedD)
              , program : (0 : Nil)
              , tempo : fromInt 1
              }    

gracedD :: List PSMusic 
gracedD = 
  singleton (PSGRACEDNOTE { graces: gracesFE
                          , graceDuration: (1 % 80) -- for each grace note
                          , note: (note (1 % 10) "D")
                          })

-- | the actual duration in grace notes is not used because it is overridden
gracesFE :: List PSNote
gracesFE = 
  (note (1 % 80) "Fs") : (note (1 % 80) "E") : Nil              



