let
  pkgs = import <nixpkgs> {};
in
  {
    moby = pkgs.haskellPackages.callPackage ./default.nix {};
  }
