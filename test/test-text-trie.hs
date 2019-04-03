
-- import qualified Data.Trie.ByteStringInternal.Test
import qualified Data.Trie.TextInternal.Test
-- import qualified Data.Trie.Test
import qualified Data.Trie.Text.Test
import qualified FromListBench
import qualified FromListBench.Text
-- import qualified TrieFile
import qualified TrieFile.Text

main :: IO ()
main = do
  -- putStrLn "Data.Trie.ByteStringInternal.Test"
  -- Data.Trie.ByteStringInternal.Test.test
  -- putStrLn "End: Data.Trie.ByteStringInternal.Test"
  -- putStrLn ""

  putStrLn "Data.Trie.TextInternal.Test"
  Data.Trie.TextInternal.Test.test
  putStrLn "End: Data.Trie.TextInternal.Test"
  putStrLn ""

  -- putStrLn "Data.Trie.Test"
  -- Data.Trie.Test.test
  -- putStrLn "End: Data.Trie.Test"
  -- putStrLn ""

  putStrLn "Data.Trie.Text.Test"
  Data.Trie.Text.Test.testText
  putStrLn "End: Data.Trie.Text.Test"
  putStrLn ""

  putStrLn "FromListBench"
  FromListBench.test
  putStrLn "End: FromListBench"
  putStrLn ""

  putStrLn "FromListBench.Text"
  FromListBench.Text.testText
  putStrLn "End: FromListBench.Text"
  putStrLn ""

  -- putStrLn "TrieFile"
  -- TrieFile.test
  -- putStrLn "End: TrieFile"
  -- putStrLn ""

  putStrLn "TrieFile.Text"
  TrieFile.Text.testText
  putStrLn "End: TrieFile.Text"
  putStrLn ""



