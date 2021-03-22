import Test.Tasty
import Tests.Notes
import Tests.Notes.Default
import Tests.Synthesizer.Structure
import Tests.Synthesizer.Oscillator

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTestsTree]

unitTestsTree :: TestTree
unitTestsTree = testGroup "UnitTests"
  [ testGroup "Tests.Notes" notesTests
  , testGroup "Tests.Notes.Default" defaultNotesTests
  , testGroup "Tests.Synthesizer.Structure" synthesizerStructureTests
  , testGroup "Tests.Synthesizer.Oscillator" oscillatorStructureTests ]