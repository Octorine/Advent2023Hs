let pkgs = import <nixpkgs>  {};
    ePkgs = pkgs.extend(self: super: {
        advent-runner = self.haskellPackages.callPackage ./advent-runner.nix {};
    });
    Advent2023Hs = ePkgs.haskellPackages.developPackage {
        root = ./.;
        modifier = (self: 
                        pkgs.haskell.lib.addBuildDepends 
                            self [pkgs.cabal-install pkgs.haskellPackages.haskell-language-server]);
    };
in Advent2023Hs
