let


  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          merkle-schemes-higher = haskellPackagesOld.callPackage ./merkle-schemes-higher/default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { merkle-schemes-higher = pkgs.haskellPackages.merkle-schemes-higher;
    merkle-schemes-higher-bt = pkgs.haskellPackages.callPackage ./merkle-schemes-higher-bt/default.nix { };
    merkle-schemes-higher-ext = pkgs.haskellPackages.callPackage ./merkle-schemes-higher-ext/default.nix { };
  }


