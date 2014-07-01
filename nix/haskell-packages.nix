# This nix-expression defines our local overrides to the nixpkgs haskellPackages.
{ pkgs, nixGenCabal } :
pkgs.haskellPackages.override {
  # The set of haskell-packages may be extended
  # open-recursive-fixpoint-/OOP-style through the 'extension' attribute.
  extension = self : super : with self;
    let cabalNix = src : cabalFile : callPackage (nixGenCabal src cabalFile).outPath {};
    in {
      typedRestTypes        = cabalNix <typed-rest/types> "typed-rest-types.cabal";
      typedRestClient       = cabalNix <typed-rest/client> "typed-rest-client.cabal";
      typedRestServer       = cabalNix <typed-rest/server> "typed-rest-server.cabal";
      encapsulatedResources = cabalNix <encapsulated-resources> "encapsulated-resources.cabal";
      muPromoteBase         = cabalNix ../base "muPromote-base.cabal";
      muPromoteNode         = cabalNix ../node "muPromote-node.cabal";
      muPromoteProcessors   = cabalNix ../processors "muPromote-processors.cabal";
      muPromoteCollectorNix = cabalNix ../collectors/nix "mupromote-collector-nix.cabal";
  };
}

