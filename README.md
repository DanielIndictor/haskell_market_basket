# Usage
From the root directory, run:
```
cabal install
cabal build
```

To run the same benchmark I did, run:
```
cabal run haskell-market-basket -- +RTS -ls -N -RTS fixtures/T40I10D100K.dat 0.01
```

# Report

See `report/report.pdf` or `report/report.md`

