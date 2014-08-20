# This nix-expression defines our local overrides to the nixpkgs haskellPackages.
{ pkgs } : self : super : with self;
  let cabalNix = src : cabalFile : super.callPackage (pkgs.nixGenCabal src cabalFile).outPath {};
  in {
    muPromoteBase         = cabalNix ../base "muPromote-base.cabal";
    muPromoteNode         = cabalNix ../node "muPromote-node.cabal";
    muPromoteProcessors   = cabalNix ../processors "muPromote-processors.cabal";
    muPromoteCollectorNix = cabalNix ../collectors/nix "mupromote-collector-nix.cabal";
  }
