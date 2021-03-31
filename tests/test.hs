import Test.Tasty
import Tests.Language
import Tests.Notes
import Tests.Notes.Default
import Tests.Synthesizer.Modifiers.Envelopes
import Tests.Synthesizer.Oscillator
import Tests.Synthesizer.Structure

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTestsTree]

unitTestsTree :: TestTree
unitTestsTree = testGroup "UnitTests"
  [ testGroup "Tests.Language" languageTests
  , testGroup "Tests.Notes" notesTests
  , testGroup "Tests.Notes.Default" defaultNotesTests
  , testGroup "Tests.Synthesizer.Structure" synthesizerStructureTests
  , testGroup "Tests.Synthesizer.Oscillator" oscillatorStructureTests
  , testGroup "Tests.Synthesizer.Modifiers.Envelopes" envelopesTests]
