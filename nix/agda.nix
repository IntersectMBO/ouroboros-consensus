inputs: final: prev:

let
  pkgs = final;

  locales = {
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LOCALE_ARCHIVE =
      if pkgs.system == "x86_64-linux"
      then "${pkgs.glibcLocales}/lib/locale/locale-archive"
      else "";
  };

  customAgda = inputs.agda-nixpkgs.legacyPackages.${pkgs.system};

  agdaStdlib = customAgda.agdaPackages.standard-library;

  agdaStdlibClasses = customAgda.agdaPackages.mkDerivation {
    inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
    pname = "agda-stdlib-classes";
    version = "2.0";
    src = pkgs.fetchFromGitHub {
      owner = "omelkonian";
      repo = "agda-stdlib-classes";
      rev = "v2.0";
      hash = "sha256-PcieRRnctjCzFCi+gUYAgyIAicMOAZPl8Sw35fZdt0E=";
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
    src = pkgs.fetchFromGitHub {
      owner = "omelkonian";
      repo = "agda-stdlib-meta";
      rev = "v2.1.1";
      hash = "sha256-qOoThYMG0dzjKvwmzzVZmGcerfb++MApbaGRzLEq3/4=";
    };
    meta = { };
    libraryFile = "agda-stdlib-meta.agda-lib";
    everythingFile = "Main.agda";
    buildInputs = [ agdaStdlib agdaStdlibClasses ];
  };

  deps = [ agdaStdlib agdaStdlibClasses agdaStdlibMeta ];
  agdaWithPkgs = p: customAgda.agda.withPackages { pkgs = p; ghc = pkgs.ghc; };

  attrs = pkgs.recurseIntoAttrs rec {
    agda = agdaWithPkgs deps;

    latex = pkgs.texlive.combine {
      inherit (pkgs.texlive)
        scheme-small
        xits
        collection-latexextra
        collection-latexrecommended
        collection-mathscience
        bclogo
        latexmk;
    };

    docs = pkgs.stdenv.mkDerivation {
      inherit (locales) LANG LC_ALL LOCALE_ARCHIVE;
      pname = "docs";
      version = "0.1";
      src = ../docs/agda-spec;
      meta = { };
      buildInputs = [ agda latex pkgs.python3 ];
      buildPhase = ''
        OUT_DIR=$out make docs
      '';
      doCheck = true;
      checkPhase = ''
        test -n "$(find $out/pdfs/ -type f -name '*.pdf')"
      '';
      dontInstall = true;
    };

    shell = pkgs.mkShell {
      packages = [ agda latex ];
    };
  };
in
{ agda-spec = attrs; }
