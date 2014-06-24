# Our local override of nixpkgs
import <nixpkgs> {
  config = {
    packageOverrides = pkgs : with pkgs; rec {
      haskellPackages = import ./haskell-packages.nix { inherit pkgs nixGenCabal; };
      nixGenCabal = callPackage ./nix-gen-cabal.nix { inherit (haskellPackages) cabal2nix; };
    };
  };
}
