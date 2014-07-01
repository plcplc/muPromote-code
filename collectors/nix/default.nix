let
  pkgs = import <nixpkgs> {};
  hsScope = pkgs.newScope pkgs.haskellPackages;
  muPromoteCollectorNix = pkgs.callPackage ./cabal2nixGen.nix
    { cabal2nix = pkgs.haskellPackages.cabal2nix; cabalFile = ./mupromote-collector-nix.cabal; srces = ./.; };
  typedRestTypes = hsScope ../../../typed-rest/types/cabal.nix {};
  typedRestClient = hsScope ../../../typed-rest/client/cabal.nix {inherit typedRestTypes;};
  typedRestServer = hsScope ../../../typed-rest/server/cabal.nix {inherit typedRestTypes;};
  encapsulatedResources = hsScope ../../../encapsulated-resources/encapsulated-resources.nix {};
  muPromoteBase = hsScope ./../../base/cabal.nix {
    inherit encapsulatedResources;
    inherit typedRestTypes;
    };
in
  hsScope (import "${muPromoteCollectorNix}") {
      inherit encapsulatedResources;
      inherit muPromoteBase;
      inherit typedRestClient;
    }
