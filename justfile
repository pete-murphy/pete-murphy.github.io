# https://just.systems

@help:
    just --list

@build:
    cabal exec build-site

@clean:
    # Clear Shake cache so build logic changes (e.g. new parsers) take effect
    rm -rf .shake

@test:
    cabal test

@preview:
    cabal exec build-site
    python3 -m http.server 8000 --directory docs