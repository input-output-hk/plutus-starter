let
  packages = import ./nix;
  inherit (packages) pkgs plutus-starter;
  project = plutus-starter.haskell.project;
in
{
  inherit pkgs plutus-starter;

  inherit project;
}
