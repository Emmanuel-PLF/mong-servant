{
  description = "mongo-servant flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/6141b8932a5cf376fe18fcd368cecd9ad946cb68";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    ekg-core = {
      flake = false;
      url = "github:hasura/ekg-core/master";
    };
    ekg-json = {
      flake = false;
      url = "github:hasura/ekg-json/master";
    };
    ekg-wai = {
      flake = false;
      url = "github:hasura/ekg-wai/master";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = [ "x86_64-linux" ]; #nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { config, pkgs, ... }: {
        # This attr is provided by https://github.com/srid/haskell-flake
        haskellProjects = 
        let 
          t = pkgs.lib.trivial;
          hl = pkgs.haskell.lib; 
        in {
          ghc8107 = {
            #name = "mongo-servant";
            root = ./.;
            haskellPackages = pkgs.haskell.packages.ghc8107;
            buildTools = hp: {
              inherit (pkgs)
                treefmt
                nixpkgs-fmt;
              inherit (hp)
                cabal-install
                #cabal-fmt
                fourmolu
                ghcid;
            };
            #modifier = drv: with pkgs.haskell.lib; dontCheck drv; # test/type-errors requires 9.2 
              modifier = (t.flip t.pipe) [
                hl.dontHaddock
                hl.enableStaticLibraries
                hl.justStaticExecutables
                hl.disableLibraryProfiling
                hl.disableExecutableProfiling
              ];
              source-overrides = {
               inherit (inputs)
                ekg-core
                ekg-json
                ekg-wai;
            };
            overrides = self': super: with pkgs.haskell.lib; {
              relude = dontCheck super.relude_1_1_0_0; # Not the default in nixpkgs yet.
              persistent-mongoDB = dontCheck (self'.callHackage "persistent-mongoDB" "2.13.0.0" { });
              persistent = dontCheck super.persistent;
              ekg-core = super.ekg-core;
              ekg-json = super.ekg-json;
              ekg-wai = super.ekg-wai;
            };
          };
        };
        devShells.default = config.devShells.ghc8107;
      };
    };
}
  
  
