{ pkgs
, sources
, plutus
, haskell-nix
, source-repo-override
}:
let
  gitignore-nix = pkgs.callPackage plutus."gitignore.nix" { };

  compiler-nix-name = plutus.plutus-apps.haskell.compiler-nix-name;

  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources haskell-nix source-repo-override;
    inherit compiler-nix-name; # Use the same GHC version as plutus
    inherit (pkgs) libsodium-vrf;
  };

  hlint = plutus.plutus-apps.hlint;

  cabal-install = plutus.plutus-apps.cabal-install;

  stylish-haskell = plutus.plutus-apps.stylish-haskell;

  haskell-language-server = plutus.plutus-apps.haskell-language-server;

  cardano-repo-tool = plutus.plutus-apps.cardano-repo-tool;
in
{
  inherit haskell hlint cabal-install stylish-haskell haskell-language-server cardano-repo-tool;
}
