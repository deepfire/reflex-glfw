{ mkDerivation, base, base-unicode-symbols, clock, dependent-sum
, GLFW-b, lens, mtl, OpenGL, OpenGLRaw, pretty, reflex, semigroups
, stdenv, stm, transformers
}:
mkDerivation {
  pname = "reflex-glfw";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-unicode-symbols clock dependent-sum GLFW-b lens mtl
    OpenGLRaw reflex semigroups stm transformers
  ];
  executableHaskellDepends = [
    base base-unicode-symbols clock dependent-sum GLFW-b lens mtl
    OpenGL OpenGLRaw pretty reflex semigroups stm transformers
  ];
  homepage = "https://github.com/deepfire/reflex-glfw";
  description = "A GLFW-b adapter for Reflex FRP";
  license = stdenv.lib.licenses.bsd3;
}
