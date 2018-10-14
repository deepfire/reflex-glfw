{ mkDerivation, base, base-unicode-symbols, containers
, dependent-sum, ghc-prim, GLFW-b, lens, mtl, OpenGL, OpenGLRaw
, pretty, primitive, ref-tf, reflex, stdenv, stm, transformers
}:
mkDerivation {
  pname = "reflex-glfw";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-unicode-symbols containers dependent-sum ghc-prim GLFW-b
    lens mtl OpenGLRaw pretty primitive ref-tf reflex stm transformers
  ];
  executableHaskellDepends = [
    base base-unicode-symbols containers dependent-sum GLFW-b lens mtl
    OpenGL OpenGLRaw pretty reflex stm transformers
  ];
  homepage = "https://github.com/deepfire/reflex-glfw/";
  description = "A GLFW-b adapter for Reflex FRP";
  license = stdenv.lib.licenses.bsd3;
}
