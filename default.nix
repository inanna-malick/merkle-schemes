let


  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          merkle-schemes-higher = haskellPackagesOld.callPackage ./merkle-schemes-higher/merkle-schemes-higher.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { merkle-schemes-higher = pkgs.haskellPackages.merkle-schemes-higher;
    merkle-schemes-higher-bt = pkgs.haskellPackages.callPackage ./merkle-schemes-higher-bt/merkle-schemes-higher-bt.nix { };
    merkle-schemes-higher-ext = pkgs.haskellPackages.callPackage ./merkle-schemes-higher-ext/merkle-schemes-higher-ext.nix { };
  }
