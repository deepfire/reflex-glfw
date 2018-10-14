{ pinned      ? true
, nixpkgs     ? import (if pinned
                        then ./nixpkgs
                        else  <nixpkgs>) {}
, compiler    ? import ./default-compiler.nix
, local       ? false
}:
let
  ghc     = import ./ghc.nix { inherit nixpkgs compiler local; };
  default = import ./default.nix;
in
  ghc.callPackage default {}
