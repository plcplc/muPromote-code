# Build the node web ui.
{ pkgs, ... } :
  pkgs.stdenv.mkDerivation {
    name = "node-web-ui.tar.xz";
    src = ./node-ui;

    # Use a bunch of nodejs/grunt/less/hs-haste/webfancy stuff:
    # buildPhase = '' ...
    # '';

    installPhase = ''
      tar -cJf $out $src
    '';
  }
