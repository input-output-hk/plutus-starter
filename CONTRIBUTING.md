# Contributing

## How to update dependencies

  1. Go to https://github.com/input-output-hk/plutus-apps/tags and take note of the latest `plutus-starter-devcontainer` tag and its commit hash.
  2. Update the tag and/or hash in the following files:
      - `.devcontainer/devcontainer.json`
      - `nix/sources.json`
      - `nix/pkgs/haskell/haskell.nix`
      - `cabal.project`
  3. Copy over most of https://github.com/input-output-hk/plutus-apps/blob/main/cabal.project into `cabal.project`
  4. Copy over the `sha256map` values from https://github.com/input-output-hk/plutus-apps/blob/main/nix/pkgs/haskell/haskell.nix into `nix/pkgs/haskell/haskell.nix` (but leave the first line that you just changed in step 2). To get the right hash value for `plutus-apps`, change one digit, then run `nix-shell`. You will get an error that shows the correct hash.
  5. It’s likely that the code for the guessing game needs to be changed; luckily, you can copy the source from https://github.com/input-output-hk/plutus-apps/blob/main/plutus-playground-server/usecases/Game.hs roughly, and it should work.
  6. Test the different parts:
      - VSCode
          * You need to make sure to rebuild the image
          * Then, once in VSCode, do `cabal build` and `cabal test`, and then make sure HLS works
      - nix
          * `nix-shell`
          * `cabal build`/`cabal test`
