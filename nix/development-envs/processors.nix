(import ./dev-env.nix) {
  envName = "muPromote-processors";
  hsEnvDeps = hsPkgs : with hsPkgs;
    muPromoteProcessors.nativeBuildInputs
    ++ muPromoteProcessors.propagatedNativeBuildInputs;
  }
