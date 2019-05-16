{ pkgs ? import <nixpkgs> {}
}:

let
  merkle-schemes-higher = pkgs.haskellPackages.callPackage ./merkle-schemes-higher/default.nix { };

in
  { merkle-schemes-higher = merkle-schemes-higher;
    merkle-schemes-higher-bt = pkgs.haskellPackages.callPackage ./merkle-schemes-higher-bt/default.nix { merkle-schemes-higher = merkle-schemes-higher; };
    merkle-schemes-higher-ext = pkgs.haskellPackages.callPackage ./merkle-schemes-higher-ext/default.nix { merkle-schemes-higher = merkle-schemes-higher; };
  }


