let
  pkgs = (import <nixpkgs>) {};
  hsScope = pkgs.newScope pkgs.haskellPackages;
in rec {

  typedRestTypes = hsScope <typed-rest/types/cabal.nix> {};
  typedRestClient = hsScope <typed-rest/client/cabal.nix> {inherit typedRestTypes;};
  typedRestServer = hsScope <typed-rest/server/cabal.nix> {inherit typedRestTypes;};
  encapsulatedResources = hsScope <encapsulated-resources/encapsulated-resources.nix> {};

  muPromoteBase = hsScope ./../base/cabal.nix {inherit typedRestTypes;};
  muPromoteNode = hsScope ./../node/cabal.nix {
    inherit encapsulatedResources;
    inherit muPromoteBase;
    inherit typedRestClient;
    inherit typedRestServer;
    inherit typedRestTypes;
    };

  muPromoteProcessors = hsScope ./../processors/cabal.nix {
    inherit encapsulatedResources;
    inherit muPromoteBase;
    inherit typedRestServer;
    inherit typedRestTypes;
    };

}
