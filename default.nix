let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    # haskellNix.sources.nixpkgs-2009
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;

in pkgs.haskell-nix.project {
  projectFileName = "cabal.project";

  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "plutus-starter";
    src = ./.;
  };
  # Specify the GHC version to use.
  compiler-nix-name = "ghc8104"; # Not required for `stack.yaml` based projects.

  modules = [
    {
      packages = {
        eventful-sql-common = {
          # This is needed so evenful-sql-common will build with a newer version of persistent.
          ghcOptions = [ "-XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses" ];
          doHaddock = false;
        };

        # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
        plutus-ledger.doHaddock = false;
        plutus-use-cases.doHaddock = false;
      };
    }
  ];
}
