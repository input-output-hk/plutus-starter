# The content of this file was partially copied from the equivalent file in the plutus repository.
# It is used by IOHK's Hydra for CI (building the project, running the tests, etc.)
#
# Therefore, do not worry too much about the structure.
let
  packages = import ./.;

  pkgs = packages.pkgs;
  haskellNix = pkgs.haskell-nix;

  # Just the packages in the project
  projectPackages = haskellNix.haskellLib.selectProjectPackages packages.project.hsPkgs;

  inherit (import ./nix/lib/ci.nix { inherit pkgs; }) dimension filterAttrsOnlyRecursive filterDerivations stripAttrsForHydra derivationAggregate;

  # Collects haskell derivations and builds an attrset:
  #
  # { library = { ... }
  # , tests = { ... }
  # , benchmarks = { ... }
  # , exes = { ... }
  # , checks = { ... }
  # }
  #  Where each attribute contains an attribute set
  #  with all haskell components of that type
  mkHaskellDimension = pkgs: haskellProjects:
    let
      # retrieve all checks from a Haskell package
      collectChecks = _: ps: pkgs.haskell-nix.haskellLib.collectChecks' ps;
      # retrieve all components of a Haskell package
      collectComponents = type: ps: pkgs.haskell-nix.haskellLib.collectComponents' type ps;
      # Given a component type and the retrieve function, retrieve components from haskell packages
      select = type: selector: (selector type) haskellProjects;
      # { component-type : retriever-fn }
      attrs = {
        "library" = collectComponents;
        "tests" = collectComponents;
        "benchmarks" = collectComponents;
        "exes" = collectComponents;
        "checks" = collectChecks;
      };
    in
    dimension "Haskell component" attrs select;

  ciJobsets = stripAttrsForHydra (filterDerivations {
    shell = (import ./shell.nix {});

    pureShell = (import ./shell.nix { pure = true; });

    build = pkgs.recurseIntoAttrs (mkHaskellDimension pkgs projectPackages);
  });
in
  ciJobsets // { required = derivationAggregate "required-plutus-starter" ciJobsets; }

