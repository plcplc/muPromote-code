{ cabal, dbus, mtl, transformers }:

cabal.mkDerivation (self: {
  pname = "muPromote-packagekit";
  version = "0.1.0.0";
  src = ./. ;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ dbus mtl transformers ];
  meta = {
    homepage = "http://www.mupromote.net";
    description = "Packagekit-enabled scraper for the μPromote funding network, plus server";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
