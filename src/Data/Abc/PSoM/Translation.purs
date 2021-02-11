-- | Translate a monophonic ABC tune into PSoM
module Data.Abc.PSoM.Translation (initialise, toPSoM) where

import Data.Abc.PSoM

import Control.Monad.State (State, get, put, evalState)
import Data.Abc (AbcRest, AbcTune, Accidental(..), Bar, BarLine, BodyPart(..), Broken(..), GraceableNote, Header(..), Mode(..), ModifiedKeySignature, Music(..), MusicLine, NoteDuration, PitchClass(..), RestOrNote, TempoSignature, TuneBody, AbcNote)
import Data.Abc.Accidentals as Accidentals
import Data.Abc.Metadata (dotFactor, getKeySig)
import Data.Abc.Midi (midiPitchOffset)
import Data.Abc.Midi.RepeatSections (initialRepeatState, indexBar, finalBar)
import Data.Abc.PSoM.RepeatBuilder (buildRepeatedMelody)
import Data.Abc.PSoM.Types (PSoMBar)
import Data.Abc.Repeats.Types (RepeatState)
import Data.Abc.Tempo (AbcTempo, getAbcTempo, defaultAbcTempo, beatsPerSecond)
import Data.Array (index) as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..), (:), null, reverse)
import Data.List.NonEmpty (length)
import Data.List.Types (NonEmptyList, toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Rational (Rational, fromInt, (%))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (bind, map, pure, ($), (+), (-), (*), (/))

-- import Debug.Trace (trace, traceShow)

-- | initialise the translation state
initialise :: AbcTune -> TransformationState
initialise =
  initialState

-- | Transform ABC into PSoM intermediate format
toPSoM :: TuneBody -> TransformationState -> PSoMProgram
toPSoM tuneBody state =
  evalState (transformBody tuneBody) state

-- | the state to thread through the computation
type TState =
    { modifiedKeySignature ::  ModifiedKeySignature    -- the current key signature
    , abcTempo ::  AbcTempo                            -- the current tempo
    , currentBar :: PSoMBar                            -- the current bar being translated
    , currentBarAccidentals :: Accidentals.Accidentals -- can't put this in PSoMBar because of typeclass constraints
                                                       -- any notes marked explicitly as accidentals in the current bar
    , lastNoteTied :: Maybe GraceableNote              -- the last note, if it was tied?
    , repeatState :: RepeatState                       -- the repeat state of the tune
    , rawTrack :: List PSoMBar                         -- the growing list of completed bars
    }

type TransformationState =
  Tuple TState PSoMProgram

-- | The very first bar
initialBar :: PSoMBar
initialBar =
  { number : 0
  , endRepeats : 0
  , startRepeats : 0
  , iteration : Nothing
  , psomMessages : Nil
  }

-- | build a new bar from a bar number and an ABC bar
buildNewBar :: Int -> BarLine -> PSoMBar
buildNewBar i barLine =
  {  number : i
  ,  endRepeats : barLine.endRepeats
  ,  startRepeats : barLine.startRepeats
  ,  iteration : barLine.iteration
  ,  psomMessages : Nil
  }

-- | default to C Major (i.e. no accidental modifiers)
defaultKey :: ModifiedKeySignature
defaultKey =
  { keySignature: { pitchClass: C, accidental: Natural, mode: Major }, modifications: Nil }

-- | this initial state is then threaded through the computation
-- | but will be altered when ABC headers are encountered
initialState :: AbcTune -> TransformationState
initialState tune =
  let
    abcTempo = getAbcTempo tune
    keySignature = fromMaybe defaultKey (getKeySig tune)
  in
    Tuple { modifiedKeySignature: keySignature
          , abcTempo : abcTempo
          , currentBar : initialBar
          , currentBarAccidentals : Accidentals.empty
          , lastNoteTied : Nothing
          , repeatState : initialRepeatState
          , rawTrack : Nil
          } mempty

transformBody :: TuneBody -> State TransformationState PSoMProgram
transformBody Nil =
  do
    finaliseMelody
transformBody (p : ps) =
  do
    _ <- transformBodyPart p
    transformBody ps

transformBodyPart :: BodyPart -> State TransformationState PSoMProgram
transformBodyPart bodyPart =
  case bodyPart of
    Score bars ->
      transformBarList bars
    BodyInfo header ->
      transformHeader header

transformBarList :: List Bar -> State TransformationState PSoMProgram
transformBarList Nil =
  do
    tpl <- get
    pure $ snd tpl
transformBarList (b : bs) =
  do
    _ <- transformBar b
    transformBarList bs

transformBar :: Bar -> State TransformationState PSoMProgram
transformBar bar =
  do
    -- save the bar to state
    _ <- updateState addBarToState bar.startLine
    transformMusicLine bar.music

transformMusicLine :: MusicLine -> State TransformationState PSoMProgram
transformMusicLine Nil =
  do
    tpl <- get
    pure $ snd tpl
transformMusicLine (l : ls) =
  do
    _ <- transformMusic l
    transformMusicLine ls

transformMusic :: Music -> State TransformationState PSoMProgram
transformMusic m =
  case m of
    Note gNote ->
      updateState (addGraceableNoteToState (1 % 1)) gNote

    Rest r ->
      updateState addRestToState r.duration

    Tuplet mGrace signature restsOrNotes ->
      updateState (addTupletToState (signature.p % signature.q)) restsOrNotes

    Chord abcChord ->
      updateState (addChordToState abcChord.duration) abcChord.notes

    BrokenRhythmPair note1 broken note2 ->
      case broken of
        LeftArrow i ->
          let
            signature1 = brokenTempo i false
            signature2 = brokenTempo i true
          in do
            _ <- updateState (addGraceableNoteToState signature1) note1
            updateState (addGraceableNoteToState signature2) note2
        RightArrow i ->
          let
            signature1 = brokenTempo i true
            signature2 = brokenTempo i false
          in do
            _ <- updateState (addGraceableNoteToState signature1) note1
            updateState (addGraceableNoteToState signature2) note2

    Inline header ->
      transformHeader header

    _ ->
      do
        tpl <- get
        pure $ snd tpl

-- | add a bar to the state.  index it and add it to the growing list of bars
addBarToState :: TState -> BarLine -> TState
addBarToState tstate barLine =
  -- the current bar held in state is empty so we coalesce
  if (isBarEmpty tstate.currentBar) then
    coalesceBar tstate barLine
  -- it's not emmpty so we initialise the new bar
  else
    let
      currentBar = tstate.currentBar
      repeatState =
        indexBar currentBar tstate.repeatState
      -- ad this bar to the growing list of bars
      rawTrack =
        -- the current bar is not empty so we aggregate the new bar into the track
        currentBar : tstate.rawTrack
    in
      tstate { currentBar = buildNewBar (currentBar.number + 1) barLine
             , currentBarAccidentals = Accidentals.empty
             , repeatState = repeatState
             , rawTrack = rawTrack
             }

-- | coalesce the new bar from ABC with the current one held in the state
-- | (which has previously been tested for emptiness)
coalesceBar :: TState -> BarLine-> TState
coalesceBar tstate barLine =
  let
    endRepeats = tstate.currentBar.endRepeats + barLine.endRepeats
    startRepeats = tstate.currentBar.startRepeats + barLine.startRepeats
    bar' = tstate.currentBar { endRepeats = endRepeats
                             , startRepeats = startRepeats
                             , iteration = barLine.iteration 
                             }
  in
    tstate { currentBar = bar' }


-- | The unit note length and tempo headers affect tempo
-- | The key signature header affects pitch
-- | other headers have no effect
-- | but ABC allows headers to change mid-tune
transformHeader :: Header -> State TransformationState PSoMProgram
transformHeader h =
  case h of
    UnitNoteLength d ->
      updateState addUnitNoteLenToState d
    Key mks _ ->
      updateState addKeySigToState mks
    Tempo t ->
      updateState addTempoToState t
    _ ->
      do
        tpl <- get
        pure $ snd tpl

-- | a note is added to the current barAccidentals as a PSNote
-- | there are other implications for state - if the note has an explicit
-- | accidental, overriding the key then it is added to state because it
-- | influences other notes later in the bar
addGraceableNoteToState :: Rational -> TState-> GraceableNote -> TState
addGraceableNoteToState modifier tstate originalgNote =
  let
    gNote = modifyNoteDuration originalgNote modifier
    Tuple msgs newTie =
      processNoteWithTie tstate gNote
    barAccidentals =
      addNoteToBarAccidentals gNote.abcNote tstate.currentBarAccidentals
  in
    tstate { currentBar = tstate.currentBar { psomMessages = msgs }
           , lastNoteTied = newTie
           , currentBarAccidentals = barAccidentals
           }

addChordToState :: Rational -> TState -> NonEmptyList AbcNote -> TState
addChordToState chordDuration tstate notes =
  let
    amendDuration :: Rational -> PSNote -> PSNote
    amendDuration signature (PSNote note) =
      PSNote ( note { duration = note.duration * signature } )
    (Tuple tstate' psNotes) = accumNotes tstate (toList notes)
    psChord = PSCHORD $ reverse $ map (amendDuration chordDuration) psNotes
  in
    case tstate.lastNoteTied of
      -- we don't support ties into chords and so emit the tied note then the chord
      -- (this is a degenerate case)
      Just lastgNote ->
        let
          lastGracedNote = buildGraceableNote tstate lastgNote
          messages = psChord : (PSGRACEABLENOTE lastGracedNote) : tstate.currentBar.psomMessages
        in
          tstate' { currentBar = tstate'.currentBar { psomMessages = messages }
                  , lastNoteTied = Nothing
                  }

      _ ->
        -- nornal case - just emit the chord
        let
          messages = psChord : tstate.currentBar.psomMessages
        in
          tstate' { currentBar = tstate'.currentBar { psomMessages = messages } }

addTupletToState :: Rational -> TState -> NonEmptyList RestOrNote -> TState
addTupletToState signature tstate abcRestOrNotes =
  let
    (Tuple tstate' psRestOrNotes) = accumRestOrNotes tstate (toList abcRestOrNotes)
    psTuplet = PSTUPLET $ PSRestOrNoteSequence
                { signature : signature
                , notes : (reverse psRestOrNotes)
                }
  in
    case tstate.lastNoteTied of
      -- we don't support ties into tuplets.  Just emit the tie first.
      Just lastgNote ->
        let
          lastGracedNote = buildGraceableNote tstate lastgNote
          messages = psTuplet : (PSGRACEABLENOTE lastGracedNote) : tstate.currentBar.psomMessages
        in
          tstate' { currentBar = tstate'.currentBar { psomMessages = messages }
                  , lastNoteTied = Nothing }
      _ ->
        let
          messages = psTuplet : tstate.currentBar.psomMessages
        in
          tstate' { currentBar = tstate'.currentBar { psomMessages = messages }  }

-- | accumulate a note for use in a chord, tuplet or broken rhythm pair
-- | accumulate the bar accidentals in state and return the built note
-- | alongside the new state
accumNote :: Tuple TState (List PSNote) -> AbcNote -> Tuple TState (List PSNote)
accumNote (Tuple tstate psNotes) abcNote =
  let
    barAccidentals =
      addNoteToBarAccidentals abcNote tstate.currentBarAccidentals
    nextNote = buildNote tstate abcNote
  in
    Tuple (tstate {currentBarAccidentals = barAccidentals }) (nextNote : psNotes)

-- | ditto for a bunch of notes
accumNotes :: TState -> List AbcNote -> Tuple TState (List PSNote)
accumNotes tstate abcNotes =
  foldl accumNote (Tuple tstate Nil) abcNotes

-- | accumulate a note/rest
accumRestOrNote :: 
  Tuple TState (List (Either PSRest PSGraceableNote)) ->
  RestOrNote ->
  Tuple TState (List (Either PSRest PSGraceableNote))
accumRestOrNote (Tuple tstate psNotes) abcRestOrNote =
  let
    barAccidentals = case abcRestOrNote of
      Left abcRest ->
        tstate.currentBarAccidentals
      Right gNote ->
        addGraceableNoteToBarAccidentals gNote tstate.currentBarAccidentals
    nextNote = case abcRestOrNote of
      Left abcRest ->
        Left $ buildRest tstate abcRest.duration
      Right gNote ->
        Right $ buildGraceableNote tstate gNote
  in
    Tuple (tstate {currentBarAccidentals = barAccidentals }) (nextNote : psNotes)

-- | ditto for a bunch of notes
accumRestOrNotes :: 
  TState -> 
  List (Either AbcRest GraceableNote) ->
  Tuple TState (List (Either PSRest PSGraceableNote))
accumRestOrNotes tstate gNotes =
  foldl accumRestOrNote (Tuple tstate Nil) gNotes

-- | process the incoming note, accounting for the fact that the previous note may have been tied.
-- |
-- | Chordal notes:
-- |
-- | we don't support ties into chords.  Just ensure the wrongly tied note is emitted
-- |
-- | Standard Notes:
-- |
-- | if it was tied, then we simply coalesce the notes by adding their durations.  If the incoming note
-- | is tied, then the (possibly combined) note is saved as the 'lastNoteTied' so that the whole
-- | process will begin again at the next note.  If not tied, then the (possibly combined) note
-- | is written into the current PSoM abcNote
-- | Grace notes are just ignored at the moment
processNoteWithTie ::  TState -> GraceableNote -> Tuple (List PSMusic) (Maybe GraceableNote)
processNoteWithTie tstate gNote =
  let 
    abcNote = gNote.abcNote 
  in 
    case tstate.lastNoteTied of
      Just lastNote ->
        let
          -- combinedAbcNote = abcNote { duration = abcNote.duration + lastNote.duration }
          combinedGraceableNote = incrementNoteDuration lastNote abcNote.duration
          psgNote = buildGraceableNote tstate combinedGraceableNote
        in
          if (abcNote.tied) then
            -- both notes tied - augment the cached tied note
            Tuple (tstate.currentBar.psomMessages) (Just combinedGraceableNote)
          else
            -- incoming note not tied - emit the augmented note
            Tuple ((PSGRACEABLENOTE psgNote) : tstate.currentBar.psomMessages) Nothing
      _  ->
        if (abcNote.tied) then
          -- the new note is tied and so cache it
          Tuple (tstate.currentBar.psomMessages) (Just gNote)
        else
          let
            psgNote = buildGraceableNote tstate gNote
          in
            -- write out the note to the current bar
            Tuple ((PSGRACEABLENOTE psgNote) : tstate.currentBar.psomMessages) Nothing

-- ! increment a note duration 
-- | used to build up tied notes
incrementNoteDuration :: GraceableNote -> NoteDuration -> GraceableNote
incrementNoteDuration gNote duration =
  let
    abcNote = gNote.abcNote
    combinedAbcNote = abcNote { duration = abcNote.duration + duration }
  in
    gNote { abcNote = combinedAbcNote }

-- | modify a note duration 
-- | for use in broken rhythm pairs etc
modifyNoteDuration :: GraceableNote -> Rational -> GraceableNote
modifyNoteDuration gNote modifier =
  let
    abcNote = gNote.abcNote
    modifiedAbcNote = abcNote { duration = abcNote.duration * modifier }
  in
    gNote { abcNote = modifiedAbcNote }    


-- Build either a normal note (returned as a grace-free GraceNote or a true graced note
buildGraceableNote :: TState -> GraceableNote -> PSGraceableNote
buildGraceableNote tstate gnote =
  case gnote.maybeGrace of  
    Nothing -> 
      let 
        note = buildNote tstate gnote.abcNote
      in
        PSGraceableNote { graces: Nil, graceDuration: (1 % 1), note}
    Just grace -> 
      let 
        graceCount = length grace.notes
        -- each grace note uses up 10% of the graced note's duration
        -- this is the summary grace duration
        graceDuration = (1 % 10) * gnote.abcNote.duration * tstate.abcTempo.unitNoteLength
        -- this is the (identical) individual grace notation, recognising that buildNote 
        -- will multiply by the unit note length itself
        individualGraceDuration = (1 % 10) * gnote.abcNote.duration 

        -- the graced note has what's left
        noteDuration = ((10 - graceCount) % 10) * gnote.abcNote.duration 
        -- set the duration of every note and build them all
        graces0 = map (\n -> n { duration = individualGraceDuration}) grace.notes
        graces = toList $ map (buildNote tstate) graces0
        note =
          buildNote tstate (gnote.abcNote { duration = noteDuration})
      in 
        PSGraceableNote { graces, graceDuration, note }

-- | Our ABC implementation uses middle C = (C,5)
-- | whereas HSoM (and thus PSoM) uses middle C = (C,4)
-- | Hence we must subtract 1 from the ABC octave
buildNote :: TState -> AbcNote -> PSNote
buildNote tstate abcNote =
  let
    length =
      tstate.abcTempo.unitNoteLength * abcNote.duration
    pitchClass = pitchString tstate abcNote
  in
    PSNote
       { pitchClass : pitchClass -- <> accidental
       , octave : abcNote.octave - 1 -- subtract because of pitch convention mismatch
       , duration : length
       }

-- | needs looking at
buildRest :: TState -> Rational -> PSRest
buildRest tstate duration =
  let
    length =
      duration * tstate.abcTempo.unitNoteLength
  in
    PSRest { duration : length }

addRestToState :: TState-> Rational -> TState
addRestToState tstate duration =
  let
    msg = PSREST $ buildRest tstate duration
    bar' = tstate.currentBar { psomMessages = (msg : tstate.currentBar.psomMessages)}
  in
    tstate { currentBar = bar' }

-- | cater for a change in key signature
addKeySigToState :: TState-> ModifiedKeySignature -> TState
addKeySigToState tstate mks =
  tstate { modifiedKeySignature = mks }

-- | cater for a change in unit note length
addUnitNoteLenToState :: TState-> Rational -> TState
addUnitNoteLenToState tstate d =
  let
    abcTempo' = tstate.abcTempo { unitNoteLength = d}
  in
    tstate { abcTempo = abcTempo' }

-- | cater for a change in tempo
addTempoToState :: TState-> TempoSignature -> TState
addTempoToState tstate tempoSig =
  let
    abcTempo' =
      tstate.abcTempo { tempoNoteLength = foldl (+) (fromInt 0) tempoSig.noteLengths
                      , bpm = tempoSig.bpm
                      }
  in
    tstate { abcTempo = abcTempo' }

-- utility functions

addGraceableNoteToBarAccidentals :: GraceableNote -> Accidentals.Accidentals -> Accidentals.Accidentals
addGraceableNoteToBarAccidentals gNote accs =
  addNoteToBarAccidentals gNote.abcNote accs

-- | if the incoming note has an explicit accidental (overriding the key signature)
-- | then add it to the accidentals in force in the current bar
addNoteToBarAccidentals :: AbcNote -> Accidentals.Accidentals -> Accidentals.Accidentals
addNoteToBarAccidentals abcNote accs =
  case abcNote.accidental of
    Implicit ->
      accs
    acc ->
      Accidentals.add abcNote.pitchClass acc accs

-- | work out the broken rhythm tempo
brokenTempo :: Int -> Boolean -> Rational
brokenTempo i isUp =
  if isUp then
    (fromInt 1) + (dotFactor i)
  else
    (fromInt 1) - (dotFactor i)

-- | does the PSoMbar hold no notes or anything else of importance
isBarEmpty :: PSoMBar -> Boolean
isBarEmpty mb =
    null mb.psomMessages

-- | generic function to update the State
-- | a is an ABC value
-- | f is a function that transforms the ABC value and adds it to the state
updateState :: forall a. (TState -> a -> TState ) -> a -> State TransformationState PSoMProgram
updateState f abc =
  do
    tpl <- get
    let
      recording = snd tpl
      tstate = fst tpl
      tstate' = f tstate abc
      tpl' = Tuple tstate' recording
    _ <- put tpl'
    pure recording

-- | move the final bar from state into the final track and then build the recording
-- | complete the RepeatState and then build the PSoM program
finaliseMelody :: State TransformationState PSoMProgram
finaliseMelody =
  do
    tpl <- get
    let
      tstate = fst tpl
      currentBar = tstate.currentBar
      -- index the final bar and finalise the repear state
      repeatState =
        finalBar currentBar tstate.repeatState
      -- ensure we incorporate the very last bar
      tstate' = tstate { rawTrack = tstate.currentBar : tstate.rawTrack
                       , repeatState = repeatState }
      -- get the program
      PSoMProgram program = buildRepeatedMelody tstate'.rawTrack tstate'.repeatState.sections
      -- get the tempo compared to the default tempo
      tempoRatio = (beatsPerSecond tstate'.abcTempo) / (beatsPerSecond defaultAbcTempo)
      -- add the title (if any)
      psomProgram = PSoMProgram $ program { tempo = tempoRatio }
      tpl' = Tuple tstate' psomProgram
    _ <- put tpl'
    pure psomProgram

pitchString :: TState -> AbcNote -> String
pitchString tstate abcNote =
  let
    pitchNumber =
      midiPitchOffset abcNote tstate.modifiedKeySignature tstate.currentBarAccidentals
  in
    fromMaybe "C" $ Array.index sharpNotes pitchNumber

sharpNotes :: Array String
sharpNotes =
  [ "C", "Cs", "D", "Ds", "E", "F", "Fs", "G", "Gs", "A", "As", "B"]

