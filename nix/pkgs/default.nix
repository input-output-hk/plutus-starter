{ pkgs
, sources
, plutus
, haskell-nix
}:
let
  inherit (pkgs) stdenv;

  gitignore-nix = pkgs.callPackage plutus."gitignore.nix" { };

  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources plutus haskell-nix;
  };

  hlint = plutus.plutus.hlint;

  cabal-install = plutus.plutus.cabal-install;

  stylish-haskell = plutus.plutus.stylish-haskell;

  haskell-language-server = plutus.plutus.haskell-language-server;

  #
  # additional haskell packages from ./nix/pkgs/haskell/extra.nix
  #
  exeFromExtras = x: haskell.extraPackages."${x}".components.exes."${x}";
  cardano-repo-tool = exeFromExtras "cardano-repo-tool";

in
{
  inherit haskell hlint cabal-install stylish-haskell haskell-language-server cardano-repo-tool;
}

