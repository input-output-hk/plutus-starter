{ lib
, haskell-nix
, gitignore-nix
, sources
, plutus
}:
let
  # Use the same index-state as plutus
  index-state = plutus.plutus.haskell.index-state;

  # Use the same GHC version as plutus
  compiler-nix-name = plutus.plutus.haskell.project.projectModule.compiler-nix-name;

  # The haskell project created by haskell-nix.cabalProject'
  project = import ./haskell.nix {
    inherit haskell-nix compiler-nix-name gitignore-nix;
  };

  # All the packages defined by our project, including dependencies
  packages = project.hsPkgs;

  # Just the packages in the project
  projectPackages = haskell-nix.haskellLib.selectProjectPackages packages;

  extraPackages = import ./extra.nix {
    inherit lib haskell-nix sources;
    inherit index-state compiler-nix-name;
  };
in
rec {
  inherit project projectPackages packages;
  inherit extraPackages;
}
