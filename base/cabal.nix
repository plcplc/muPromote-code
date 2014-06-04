{ aeson, cabal, cereal, encapsulatedResources, hspec, httpClient, network, safecopy, stm, typedRestTypes }:

cabal.mkDerivation (self: {
  pname = "muPromote-base";
  version = "0.1.0.0";
  buildDepends = [
    aeson cereal encapsulatedResources hspec httpClient network
    safecopy stm typedRestTypes
    ];
  src = ./.;
  meta = {
    homepage = "http://mupromote.net";
    description = "Common types and definitions for μPromote applications";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
