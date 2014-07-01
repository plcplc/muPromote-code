(import ./dev-env.nix) {
  envName = "collector-nix";
  hsEnvDeps = hsPkgs : with hsPkgs.muPromoteCollectorNix;
    builtins.concatLists [
      buildInputs
      extraBuildInputs
      nativeBuildInputs
      propagatedBuildInputs
      propagatedNativeBuildInputs
      ];
  }
