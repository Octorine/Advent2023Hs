let
  pkgs = import <nixpkgs> { };
  ePkgs = pkgs.extend (self: super: {
    advent-runner = self.haskellPackages.callPackage ./advent-runner.nix { };
  });
  Advent2023Hs = ePkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = (self:
      pkgs.haskell.lib.addBuildTools (pkgs.haskell.lib.addBuildDepends self [
        pkgs.cabal-install
        pkgs.haskellPackages.haskell-language-server
      ]) [ pkgs.emacsPackages.lsp-haskell ]);
  };
in Advent2023Hs
