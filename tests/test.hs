import Test.Tasty
import Tests.Notes

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTestsTree]

unitTestsTree :: TestTree
unitTestsTree = testGroup "UnitTests" [unitTests]

unitTests = testGroup "(HUnit)" notesTests