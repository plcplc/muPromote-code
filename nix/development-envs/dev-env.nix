# This nix environment contains all the tools used in the process of developing
# μPromote.
# Because each package has its own dependencies, this file provides a function
# for creating environments, rather than a concrete environment.
# envName :: String -- The suffix of the name of the resulting environment.
#   'mupromote-envName'.
# hsEnvDeps :: HsPkgs -> [CabalNix] -- the dependencies of the package being
#   developed in this environment.
{ envName, hsEnvDeps } :
let
  pkgs = import ../nixpkgs.nix;
  hsEnv = pkgs.haskellPackages.ghcWithPackagesOld (hsPkgs : [
    hsPkgs.hlint
    hsPkgs.ghcMod
    hsPkgs.hdevtools
    hsPkgs.hasktags
    ] ++ (hsEnvDeps hsPkgs));
in
  pkgs.myEnvFun rec {
    name = envName;
    buildInputs = with pkgs; [
      binutils
      coreutils
      ctags
      vimHugeX
      zsh
      firefox
      hsEnv
      ];
    shell = "${pkgs.zsh.outPath}/bin/zsh";
    extraCmds = ''
      $(grep export ${hsEnv.outPath}/bin/ghc)
    '';
    }
