{ self, super, pkgs, lib, local ? false }:

with pkgs.haskell.lib; with lib; with self; {

  monoidal-containers = overrideCabal super.monoidal-containers (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "obsidiansystems";
      repo   = "monoidal-containers";
      rev    = "79c25ac6bb469bfa92f8fd226684617b6753e955";
      sha256 = "0j2mwf5zhz7cmn01x9v51w8vpx16hrl9x9rcx8fggf21slva8lf8";
    };
    jailbreak       = true;
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ aeson these ]);
  });

  reflex = overrideCabal super.reflex (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./reflex/reflex.src.json)) ["date"]);
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ lens data-default hlint filemanip monad-control monoidal-containers prim-uniq unbounded-delays MemoTrie ]);
    jailbreak       = true;
  });
}
