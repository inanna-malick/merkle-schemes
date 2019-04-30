let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./merkle-schemes.nix { }
