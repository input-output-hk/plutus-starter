{ lib
, haskell-nix
, gitignore-nix
, sources
, compiler-nix-name
, libsodium-vrf
, source-repo-override
}:
let
  # The Hackage index-state from cabal.project
  index-state =
    let
      parseIndexState = rawCabalProject:
        let
          indexState = lib.lists.concatLists (
            lib.lists.filter (l: l != null)
              (map (l: builtins.match "^index-state: *(.*)" l)
                (lib.splitString "\n" rawCabalProject)));
        in
        lib.lists.head (indexState ++ [ null ]);
    in
    parseIndexState (builtins.readFile ../../../cabal.project);

  # The haskell project created by haskell-nix.cabalProject'
  project = import ./haskell.nix {
    inherit lib haskell-nix compiler-nix-name gitignore-nix libsodium-vrf source-repo-override;
  };

  # All the packages defined by our project, including dependencies
  packages = project.hsPkgs;

  # Just the packages in the project
  projectPackages = haskell-nix.haskellLib.selectProjectPackages packages;
in
rec {
  inherit project projectPackages packages;
}
