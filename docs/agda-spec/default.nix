{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {
    overlays = [ ];
    config = { };
  }
}:

with pkgs;
let
  locales = {
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux"
                     then "${pkgs.glibcLocales}/lib/locale/locale-archive"
                     else "";
  };

  customAgda = import sources.agda-nixpkgs {
    inherit (pkgs) system;
  };

  agdaStdlib = customAgda.agdaPackages.standard-library;

  agdaStdlibClasses = customAgda.agdaPackages.mkDerivation {
    inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
    pname = "agda-stdlib-classes";
    version = "2.0";
    src = fetchFromGitHub {
      repo = "agda-stdlib-classes";
      owner = "omelkonian";
      rev = "v2.0";
      sha256 = "4ujdQv38u6BybFhRup9PMR0xpI59J/Naz/kaBtQQ9aY=";
    };
    meta = { };
    libraryFile = "agda-stdlib-classes.agda-lib";
    everythingFile = "Classes.agda";
    buildInputs = [ agdaStdlib ];
  };

  agdaStdlibMeta = customAgda.agdaPackages.mkDerivation {
    inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
    pname = "agda-stdlib-meta";
    version = "2.0";
    src = fetchFromGitHub {
      repo = "stdlib-meta";
      owner = "input-output-hk";
      rev = "4fc4b1ed6e47d180516917d04be87cbacbf7d314";
      sha256 = "T+9vwccbDO1IGBcGLjgV/fOt+IN14KEV9ct/J6nQCsM=";
    };
    meta = { };
    libraryFile = "agda-stdlib-meta.agda-lib";
    everythingFile = "Main.agda";
    buildInputs = [ agdaStdlib agdaStdlibClasses ];
  };

  deps = [ agdaStdlib agdaStdlibClasses agdaStdlibMeta ];
  agdaWithPkgs = p: customAgda.agda.withPackages { pkgs = p; ghc = pkgs.ghc; };

in
rec {

  agdaWithDeps = agdaWithPkgs deps;
  agda = agdaWithPkgs (deps ++ [ formalLedger ]);

  latex = texlive.combine {
    inherit (texlive)
      scheme-small
      xits
      collection-latexextra
      collection-latexrecommended
      collection-mathscience
      bclogo
      latexmk;
  };

  formalLedger = customAgda.agdaPackages.mkDerivation {
    inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
    pname = "formal-ledger";
    version = "0.1";
    src = ./src;
    meta = { };
    buildInputs = deps;
    buildPhase = ''
      bash typeCheck.sh
    '';
    postInstall = ''
      cp -r latex/ Makefile typecheck.time $out
      sh checkTypeChecked.sh
    '';
    extraExtensions = [ "hs" "cabal" "py" ];
  };

  mkSpecDerivation = { project, main }: rec {
    docs = stdenv.mkDerivation {
      inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
      pname = "docs";
      version = "0.1";
      src = "${formalLedger}";
      meta = { };
      buildInputs = [ agdaWithDeps latex python3 ];
      buildPhase = ''
        OUT_DIR=$out make "${project}".docs
      '';
      doCheck = true;
      checkPhase = ''
        test -n "$(find $out/pdfs/ -type f -name '*.pdf')"
      '';
      dontInstall = true;
    };

    html = stdenv.mkDerivation {
      inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
      pname = "html";
      version = "0.1";
      src = "${formalLedger}";
      meta = { };
      buildInputs = [ agdaWithDeps ];
      buildPhase = ''
        OUT_DIR=$out make "${project}".html
      '';
      doCheck = true;
      checkPhase = ''
        test -n "$(find $out/html/ -type f -name '*.html')"
      '';
      dontInstall = true;
    };

    hsSrc = stdenv.mkDerivation {
      inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
      pname = "hs-src";
      version = "0.1";
      src = "${formalLedger}";
      meta = { };
      buildInputs = [ agdaWithDeps ];
      buildPhase = ''
        OUT_DIR=$out make "${project}".hs
      '';
      doCheck = true;
      checkPhase = ''
        test -n "$(find $out/haskell/ -type f -name '*.hs')"
        # OUT_DIR=$out make "${project}".hsTest
      '';
      dontInstall = true;
    };

    # hsDocs = stdenv.mkDerivation {
    #   pname = "hs-docs";
    #   version = "0.1";
    #   src = "${formalLedger}";
    #   meta = { };
    #   buildInputs = [ agdaWithDeps ];
    #   configurePhase = ''
    #     export HOME=$TMP
    #   '';
    #   buildPhase = ''
    #     OUT_DIR=$out make "${project}".hsDocs
    #   '';
    #   doCheck = true;
    #   checkPhase = ''
    #     test -n "$(find $out/haskell/ -type f -name '*.html')"
    #   '';
    #   dontInstall = true;
    # };

    hsExe = haskell.lib.disableLibraryProfiling (haskellPackages.callCabal2nixWithOptions "${project}" "${hsSrc}/haskell/${main}" "--no-haddock" {});

  };

  mkPdfDerivation = name: version: project: stdenv.mkDerivation {
    inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
    pname = name;
    version = version;
    src = "${formalLedger}";
    meta = { };
    buildInputs = [ agdaWithDeps latex python3 ];
    buildPhase = ''
        OUT_DIR=$out make ${project}
      '';
    doCheck = true;
    checkPhase = ''
        test -n "$(find $out/pdfs/ -type f -name '*.pdf')"
      '';
    dontInstall = true;
  };

  ledger = mkSpecDerivation {
    project = "ledger";
    main = "Ledger";
  } // {
    conway = mkPdfDerivation "conway-formal-spec" "0.9" "ledger.conway.docs";
  };
  midnight = mkSpecDerivation {
    project = "midnight";
    main = "MidnightExample";
  };
}
