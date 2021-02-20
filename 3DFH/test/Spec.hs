import Lib
import Criterion.Main

comparemakeARGB :: Benchmark
comparemakeARGB = bgroup "comparemakeARGB" [bench "makeARGB" $ whnf (makeARGB 45 29 40) 98{- , bench "makeARGB2" $ whnf (makeARGB2 45 29 40) 98 -}]

comparegetARGB :: Benchmark
comparegetARGB = bgroup "comparegetARGB" [bench "getARGB" $ whnf getARGB 0x12345678{- , bench "getARGB0" $ whnf getARGB0 0x12345678 -}]

main :: IO ()
main = defaultMain [comparemakeARGB, comparegetARGB]
