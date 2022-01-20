{ plutus-apps ? null
}:

# The content of this file was partially copied from the equivalent file in the plutus repository.
# It is used by IOHK's Hydra for CI (building the project, running the tests, etc.)
#
# Therefore, do not worry too much about the structure.
let
  # If hydra passed us a plutus-apps checkout, we want to override the
  # plutus-apps used by haskell.nix
  source-repo-override = if plutus-apps == null then {} else {
    # Overwrite the source-repository-package entry with this URL
    "https://github.com/input-output-hk/plutus-apps.git" = orig: {
      url = plutus-apps.uri;
      ref = plutus-apps.rev;
      # Nix needs the sha256 of the checked-out source, but hydra
      # gives us the path itself, not its hash. This uses the
      # exportReferencesGraph feature of Nix to introspect properties
      # of a given store path (in this case, its sha256), creates
      # a Nix expression containing that hash, and imports that Nix
      # expression into the current evaluation to get the hash
      sha256 = import (pkgs.stdenv.mkDerivation {
        name = "plutus-apps-sha.nix";
        exportReferencesGraph.plutus-apps = plutus-apps;
        __structuredAttrs = true;
        PATH = pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.jq ];
        builder = builtins.toFile "builder" ''
          . .attrs.sh
          jq '."plutus-apps"[0].narHash' < .attrs.json > "$(jq -r .outputs.out < .attrs.json)"
        '';
      });
      # We assume the new version has the same subpackages we did
      # originally.
      inherit (orig) subdirs;
    };
  };
  packages = import ./. {
    inherit source-repo-override;
  };

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
    shell = (import ./shell.nix { inherit source-repo-override; });

    pureShell = (import ./shell.nix { pure = true; inherit source-repo-override; });

    build = pkgs.recurseIntoAttrs (mkHaskellDimension pkgs projectPackages);
  });
in
  ciJobsets // { required = derivationAggregate "required-plutus-starter" ciJobsets; }

