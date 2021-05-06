let
  sources = import ./sources.nix { inherit pkgs; };

  plutus = import sources.plutus {};
  pkgs = plutus.pkgs;

  haskell-nix = pkgs.haskell-nix;

  plutus-starter = import ./pkgs {
    inherit pkgs haskell-nix sources plutus;
  };

in
{
  inherit pkgs plutus plutus-starter;
}
