# Compile application in src directory
cd src
ghc Main.hs -prof -rtsopts
cd ..

# build index
# rm index.db
# src/Main index datasets/reuters-90/test -r


# Search for term on index
time src/Main search aluminium +RTS -p -hc -K3G
