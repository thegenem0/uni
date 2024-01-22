FROM haskell:8.10

WORKDIR /app

COPY ./app /app

RUN cabal update && cabal install --lib random && cabal install --lib containers

RUN ghc -O2 -o functional Main.hs
