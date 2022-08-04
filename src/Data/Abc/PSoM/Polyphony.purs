module Data.Abc.PSoM.Polyphony 
  ( generateDSL
  , generateDSL') where

import Data.Abc (AbcTune, TuneBody)
import Data.Abc.Metadata (getTitle)
import Data.Abc.PSoM.DSL (toDSL)
import Data.Abc.PSoM.Translation (initialise, toPSoM)
import Data.Abc.Voice (partitionTuneBody)
import Data.Array (index)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, foldl1, length, mapWithIndex) as NEA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi.Instrument (InstrumentName(..))
import Prelude (($), (>), (<>), map)

-- | Discriminate between monophonic and polyphonic ABC tunes and generate
-- | appropriate PSoM DSL.  The latter requires separate strands for each voice
-- | which are joined together at the start by means of a Par construct
-- | generate the PSoM DSL for the ABC tune using the provided instruments
generateDSL :: AbcTune -> Array InstrumentName -> String
generateDSL  abcTune instrumentNames =
  let
    voices = partitionTuneBody abcTune
    tuneName = entitle $ getTitle abcTune
  in
    if (NEA.length voices) > 1 then
      -- polyphonic
      let
        voicesArray :: NonEmptyArray String
        voicesArray = generateVoices abcTune voices instrumentNames
      in
        tuneName <> "\r\n" <> "Par\r\n" <> (NEA.foldl1 (<>) voicesArray)
    else
      -- monophonic
      tuneName <> "\r\n" <> (generateVoice instrumentNames abcTune 0 abcTune.body)

-- | As above, but generate the PSoM DSL for the separate ABC voices and tune name 
-- | using the provided instruments
generateDSL' :: NonEmptyArray AbcTune -> Array InstrumentName -> String -> String
generateDSL' tunes instrumentNames name =
  case (NEA.length tunes) of 
    1 ->
      -- monophonic
      let 
        abcTune = NEA.head tunes 
      in
        (enquote name) <> "\r\n" <> (generateVoice instrumentNames abcTune 0 abcTune.body)    
    _ -> 
      -- polyphonic
      let
        abcTune = NEA.head tunes 
        voices = map (\t -> t.body) tunes
        voicesArray :: NonEmptyArray String
        voicesArray = generateVoices abcTune voices instrumentNames
      in
        (enquote name) <> "\r\n" <> "Par\r\n" <> (NEA.foldl1 (<>) voicesArray)  


-- | generate the DSL for all the polyphonic voices
generateVoices :: AbcTune -> NonEmptyArray TuneBody -> Array InstrumentName -> NonEmptyArray String
generateVoices abcTune tuneBodies instrumentNames =
  NEA.mapWithIndex (generateVoice instrumentNames abcTune) tuneBodies

-- | generate PSoM DSL for a single voice
-- | note that for monophonic tunes, TuneBody is identical to AbcTune.body
-- | but for polyohonic tunes it is not
-- | (Slightly awkward because initialise only uses the headers but the API
-- | from Abc is expressed in terms of the overall tune.)
generateVoice :: Array InstrumentName -> AbcTune -> Int -> TuneBody -> String
generateVoice instrumentNames abcTune ix  tuneBody =
  let
    transformationState = initialise abcTune
    instrumentName = fromMaybe AcousticGrandPiano $
      index instrumentNames ix
  in
    toDSL (toPSoM tuneBody transformationState) instrumentName

-- | get the quoted title of the ABC tune ('unttitled' if not title present)
entitle :: Maybe String -> String
entitle Nothing = (enquote "untitled")
entitle (Just name) = (enquote name)

enquote :: String -> String
enquote s =
  "\"" <> s <> "\""
