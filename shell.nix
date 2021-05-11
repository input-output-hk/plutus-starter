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
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;

  inherit (pkgs) lib;
  inherit (pkgs.haskell-nix.haskellLib) selectProjectPackages;

  project = import ./default.nix;
in
  project.shellFor {
    packages = ps: lib.attrValues (selectProjectPackages ps);

    buildInputs = with pkgs; [ jq ];

    exactDeps = true;

    tools = {
      cabal = "3.2.0.0";
      hlint = "latest";
      stylish-haskell = "latest";
      haskell-language-server = "latest";
    };

    nativeBuildInputs = [
      # Used by scripts/bin/cabal
      (pkgs.bubblewrap.overrideAttrs (old: {
        patches = old.patches or [] ++ [
          # https://github.com/containers/bubblewrap/pull/402
          # Patch for bubblewrap to forward SIGINT (Ctrl-C) to the running
          # process, allowing Ctrl-C in cabal repl to properly clear the
          # current line
          (pkgs.fetchpatch {
            url = "https://github.com/containers/bubblewrap/pull/402/commits/77bc87e6f9042000a56091539ce2ca66660cd068.patch";
            sha256 = "08psqg7bkapg9cgipszjs6xh6pcjcg0la6p5rp4abi7il6cyj0fj";
          })
        ];
      }))
    ];

    shellHook = ''
      source scripts/wrap-cabal/wrap-cabal.sh
    '';
  }
