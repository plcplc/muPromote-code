{ aeson, cabal, conduit, dataDefault, encapsulatedResources, hspec, httpClient,
  httpTypes, muPromoteBase, stm, transformers, typedRestServer, typedRestTypes,
  wai, waiAppStatic, waiTest, warp }:

cabal.mkDerivation (self: {
  pname = "muPromote-processors";
  version = "0.1.0.0";
  buildDepends = [
    aeson conduit dataDefault encapsulatedResources httpClient httpTypes
    muPromoteBase stm transformers typedRestServer typedRestTypes wai
    waiAppStatic warp
    ];
  testDepends = [
    aeson conduit dataDefault encapsulatedResources httpClient httpTypes hspec
    transformers typedRestServer typedRestTypes wai waiTest
    ];
  src = ./.;
  meta = {
    homepage = "http://mupromote.org";
    description = " μPromote processors are the component of μPromote that actually delivers promotions.";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
