let

    # Some constant configuration that's factored separately.
    #
    config = import ./config.nix;

    # For reproducibility we'll use this project's pinned Nixpkgs instead of
    # `(import <nixpkgs> {})`.  This way we get a specific version of Nixpkgs
    # instead of a version that happens to be in the environment's `NIX_PATH`.
    #
    external = import ../nix/external;
    nixpkgs = (import external.nixpkgs-unstable) {
        # We don't want user configuration affecting this build.  This is
        # recommended boilerplate when importing Nixpkgs.
        config   = {
             allowUnfree = true;
             allowBroken = true;
             cudaSupport = true;
        };  # to avoid picking up ~/.config/nixpkgs/config.nix
        overlays = [];  # to avoid picking up ~/.config/nixpkgs/overlays
    };

    # This is the build of HLS for this project.
    #
    hls = import ../. {
        ghcVersion = config.hlsGhcVersion;
        hlsUnstable   = config.hlsUnstable;
    };

    # This function helps filter source to just what's needed.  This way, any
    # intermediate files created while developing don't affect Nix hash
    # calculations, which could result in cache misses against /nix/store.
    #
    cleaned = path: nixpkgs.lib.sourceFilesBySuffices path
        [".hs" ".lhs" ".cabal"];

    # `packageSourceOverrides` is not documented widely.  The best documentation
    # is in Nixpkgs' source.  This call helps us map our plain Cabal projects to
    # an attribute name.  We end up with a function we can use to extend the
    # Nixpkgs build of Haskell packages with the build of our project.  This
    # allows our build to reference dependencies already in Nixpkgs.
    #
    # Note that for each package in our Cabal project, we need an entry.  And
    # the attribute names need to match the package names referenced in the
    # Cabal file.
    #
    # Also, note that this function is aware of a very large percentange of
    # non-Haskell dependencies needed by Hackage packages.  If you have a
    # Hackage dependency in your Cabal file that requires a non-Haskell
    # dependency (as is the case with example-haskell-app pulling text-icu),
    # then packageSourceOverrides should pull in what's needed automatically.
    #
    overrides = nixpkgs.haskell.lib.packageSourceOverrides {
      mongo-servant = cleaned ./.;
      #persistent         = "2.11.0.2";
      persistent-mongoDB = "2.13.0.1";
      #persistent-test    = "2.0.3.5";
      #persistent-sqlite = "2.11.1.0";
      #persistent-template = "2.9.1.0";
    };

    # Here we extend Nixpkgs' build of Haskell packages for a specific version
    # of GHC.  The resultant set of Haskell packages includes our example
    # project's packages.
    #
    haskellPackages =
      nixpkgs.haskell.packages.${config.nixpkgsGhcVersion}.extend (selfH: superH: {
               mongo-servant = 
                   let mypkg = selfH.callCabal2nix "mongo-servant" ./. {};
                   in nixpkgs.haskell.lib.overrideCabal mypkg (old: {
                       enableSharedExecutables = false;
                       enableSharedLibraries = false;
                       #configureFlags = [
                       #"--ghc-option=-optl=-static"
                       #"--ghc-option=-optl=-L${nixpkgs.gmp6.override { withStatic = true; }}/lib"
                       #"--ghc-option=-optl=-L${nixpkgs.zlib.static}/lib"
                       #"--ghc-option=-optl=-L${nixpkgs.glibc.static}/lib"
                       #];
                    });

               persistent-mongoDB = nixpkgs.haskell.lib.dontCheck (selfH.callHackage "persistent-mongoDB" "2.13.0.0" {});
               #aeson              = selfH.callHackage "aeson" "1.5.6.0" {};
               #hashable-time      y
               ekg-core = selfH.callCabal2nix "ekg-core" external.ekg-core {};
               #ekg-json = selfH.callCabal2nix "ekg-json" ./ekg-json {};
               ekg-wai = selfH.callCabal2nix "ekg-wai" external.ekg-wai {};
               persistent = nixpkgs.haskell.lib.dontCheck superH.persistent;
               ghcid =  selfH.callHackage "ghcid" "0.8.7" {};
               ekg-json = selfH.callCabal2nix "ekg-json" external.ekg-json {};
              });


    # This function selects out packages for our example project from a set of
    # all Haskell packages.
    #
    #
    selectPackages = hsPkgs: with hsPkgs; {
        inherit
        mongo-servant 
        ghcid
        hlint
        ormolu
        ;
       
    };

    # This is an attribute set of derivations for our example project to be used
    # in a `default.nix` file.
    #
    project = selectPackages haskellPackages;

    # This is a derivation we can use with `nix-shell` to develop our project.
    # With `nix-shell` we'll get a shell environment with `PATH` set up with
    # HLS, Cabal, and a GHC loaded with all the packages we need as dependencies
    # (but not including any of the packages of our example project).
    #
    shell = haskellPackages.shellFor {

        # The transitive closure of dependencies of these packages are put in
        # the package database provided with GHC, with the exception of the
        # packages themselves.
        #
        # For instance, we we only selected out example-haskell-app, but not
        # example-haskell-lib, then example-haskell-lib would end up in the
        # package database.  However, this would be inconsequential unless we
        # removed example-haskell-lib's building from the cabal.project file.
        # Everything in cabal.project is built locally by Cabal, whether there
        # is something useable in a package database or not.
        #
        packages = hsPkgs: builtins.attrValues (selectPackages hsPkgs);

        # These are extra tools beyond GHC that we want on our PATH in our Nix
        # shell.
        #
        buildInputs = [
            hls.cabal-install
            #hls.hls-renamed
            #hls.hls-wrapper
            #hls.implicit-hie
            nixpkgs.nodejs
            nixpkgs.git
            #nixpkgs.atom
            #nixpkgs.jq
            #nixpkgs.mongodb-compass
            nixpkgs.zlib
            project.ghcid
            project.hlint
            project.ormolu
            #nixpkgs.libpqxx
        ];

    };

in { inherit project shell; }
