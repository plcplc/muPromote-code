(import ./dev-env.nix) {
  envName = "node";
  hsEnvDeps = hsPkgs : with hsPkgs;
    muPromoteNode.nativeBuildInputs
    ++ muPromoteNode.propagatedNativeBuildInputs;
  }
