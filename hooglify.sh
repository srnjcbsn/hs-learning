cabal haddock --hoogle
mv dist/doc/html/hs-learning/hs-learning.txt default.txt
hoogle convert default.txt
rm default.txt

