# Contributing

## How to update dependencies

  1. Go to https://github.com/input-output-hk/plutus-apps/tags and take note of the latest `v.X.Y.Z` tag.
  2. Update the tag and/or hash in the following files:
      - `.devcontainer/devcontainer.json`
      - `cabal.project`
  3. Copy over most of https://github.com/input-output-hk/plutus-apps/blob/main/cabal.project into
     `cabal.project`.
  4. It’s likely that the code for the guessing game needs to be changed;
     the example is not sync with plutus-apps anymore, so it has to be fixed locally.
  5. Test the different parts:
      - VSCode
          * You need to make sure to rebuild the image
          * Then, once in VSCode, do `cabal build` and `cabal test`, and then make sure HLS works
      - nix
          * `nix-shell`
          * `cabal build`/`cabal test`
