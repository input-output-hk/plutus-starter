{ pkgs
, sources
, plutus
, haskell-nix
}:
let
  gitignore-nix = pkgs.callPackage plutus."gitignore.nix" { };

  compiler-nix-name = plutus.plutus.haskell.compiler-nix-name;

  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources haskell-nix;
    inherit compiler-nix-name; # Use the same GHC version as plutus
    inherit (pkgs) libsodium-vrf;
  };

  hlint = plutus.plutus.hlint;

  cabal-install = plutus.plutus.cabal-install;

  stylish-haskell = plutus.plutus.stylish-haskell;

  haskell-language-server = plutus.plutus.haskell-language-server;

  cardano-repo-tool = plutus.plutus.cardano-repo-tool;
in
{
  inherit haskell hlint cabal-install stylish-haskell haskell-language-server cardano-repo-tool;
}
