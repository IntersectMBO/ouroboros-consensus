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
    src = pkgs.fetchFromGitHub {
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
