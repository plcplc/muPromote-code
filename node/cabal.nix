{ aeson, cabal, conduit, dataDefault, encapsulatedResources, hspec, httpClient,
  httpTypes, muPromoteBase, stm, transformers, typedRestClient, typedRestTypes,
  typedRestServer, wai, waiAppStatic, waiTest, warp }:

cabal.mkDerivation (self: {
  pname        = "muPromote-node";
  version      = "0.1.0.0";
  buildDepends = [
    aeson conduit dataDefault encapsulatedResources httpClient httpTypes
    muPromoteBase stm transformers typedRestClient typedRestTypes
    typedRestServer wai waiAppStatic warp
    ];
  testDepends  = [
    aeson conduit dataDefault encapsulatedResources httpClient httpTypes hspec
    muPromoteBase transformers typedRestClient typedRestTypes typedRestServer
    wai waiTest
    ];
  src  = ./.;
  meta = {
    homepage    = "http://mupromote.org";
    description = "The core library and executable for the muPromote node";
    license     = self.stdenv.lib.licenses.gpl3;
    platforms   = self.ghc.meta.platforms;
  };
})
