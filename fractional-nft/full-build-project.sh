cabal clean
cabal update
cabal build -w ghc-8.10.4 -O2
cabal run fractional-nft
echo "DONE"