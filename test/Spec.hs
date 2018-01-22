import qualified TestHS as T
import Test.NMA as N

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTestsIO $ N.ioTests 
