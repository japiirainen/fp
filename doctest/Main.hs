import qualified Test.DocTest

main :: IO ()
main = Test.DocTest.doctest ["--fast", "src"]
