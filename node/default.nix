let
  pkgs = (import <nixpkgs>) {}; #(import ../../../tmp/nixpkgs) {};
  hsScope = pkgs.newScope pkgs.haskellPackages;
  cabalPackage = import ./cabal.nix;
  cabalDeriv = hsScope cabalPackage {};
in
  # build mupromote-core with tests enabled
  pkgs.lib.overrideDerivation cabalDeriv
    ({extraConfigureFlags, ...} :
      {extraConfigureFlags = ["--enable-tests"];})
