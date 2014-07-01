(import ./dev-env.nix) {
  envName = "muPromote-base";
  hsEnvDeps = hsPkgs : with hsPkgs;
    muPromoteBase.nativeBuildInputs
    ++ muPromoteBase.propagatedNativeBuildInputs;
  }
