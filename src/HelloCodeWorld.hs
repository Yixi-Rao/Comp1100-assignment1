import CodeWorld
import Data.Text (Text, pack)

main :: IO()
main = drawingOf p
  where
    p = (text t) :: Picture
    t = (pack s) :: Text
    s = "Hello World!" :: String
