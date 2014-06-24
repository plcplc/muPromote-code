let
  pkgs = import ./nixpkgs.nix;
  hsScope = pkgs.newScope pkgs.haskellPackages;

in with pkgs.haskellPackages; {
  # is hsScope really necessary?

  typedRestTypes = typedRestTypes;
  typedRestClient = typedRestClient;
  typedRestServer = typedRestServer;
  encapsulatedResources = encapsulatedResources;

  muPromoteBase = muPromoteBase;
  muPromoteNode = muPromoteNode;

  muPromoteProcessors = muPromoteProcessors;
  muPromoteCollectorNix  = muPromoteCollectorNix;

}
