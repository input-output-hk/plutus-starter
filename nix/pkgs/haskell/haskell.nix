############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, gitignore-nix
, compiler-nix-name
}:

let
  project = haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = haskell-nix.haskellLib.cleanGit {
      name = "plutus-starter";
      src = ../../../.;
    };

    inherit compiler-nix-name;

    sha256map = {
      "https://github.com/Quid2/flat.git"."95e5d7488451e43062ca84d5376b3adcc465f1cd" = "06l31x3y93rjpryvlxnpsyq2zyxvb0z6lik6yq2fvh36i5zwvwa3";
      "https://github.com/input-output-hk/plutus.git"."514f059a7aabc4b03ca80343d29cf0736f9ea498" = "0l848y4y6yqva1xfcv05dyhb6hhvlmnjcign4dwmf1cgy0kyz4v5";
      "https://github.com/shmish111/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596" = "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";
      "https://github.com/shmish111/servant-purescript.git"."a76104490499aa72d40c2790d10e9383e0dbde63" = "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";
      "https://github.com/input-output-hk/cardano-crypto.git"."f73079303f663e028288f9f4a9e08bcca39a923e" = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
      "https://github.com/input-output-hk/cardano-base"."4251c0bb6e4f443f00231d28f5f70d42876da055" = "02a61ymvx054pcdcgvg5qj9kpybiajg993nr22iqiya196jmgciv";
      "https://github.com/input-output-hk/cardano-prelude"."ee4e7b547a991876e6b05ba542f4e62909f4a571" = "0dg6ihgrn5mgqp95c4f11l6kh9k3y75lwfqf47hdp554w7wyvaw6";
      "https://github.com/input-output-hk/ouroboros-network"."6cb9052bde39472a0555d19ade8a42da63d3e904" = "0rz4acz15wda6yfc7nls6g94gcwg2an5zibv0irkxk297n76gkmg";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."34abfb7f4f5610cabb45396e0496472446a0b2ca" = "1fdc0a02ipa385dnwa6r6jyc8jlg537i12hflfglkhjs2b7i92gs";
      "https://github.com/input-output-hk/cardano-ledger-specs"."097890495cbb0e8b62106bcd090a5721c3f4b36f" = "0i3y9n0rsyarvhfqzzzjccqnjgwb9fbmbs6b7vj40afjhimf5hcj";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
    };

    modules = [
      {
        packages = {
          eventful-sql-common = {
            # This is needed so evenful-sql-common will build with a newer version of persistent.
            ghcOptions = [ "-XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses" ];
            doHaddock = false;
          };

          # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
          plutus-ledger.doHaddock = false;
          plutus-use-cases.doHaddock = false;
        };
      }
    ];
  };
in
  project

