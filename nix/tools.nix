inputs: final: prev:

let
  tool-index-state = "2023-07-05T00:00:00Z";
  tool = name: version: other:
    final.haskell-nix.tool final.hsPkgs.args.compiler-nix-name name ({
      version = version;
      index-state = tool-index-state;
    } // other);
in
{
  inherit tool-index-state;

  cabal = tool "cabal" "latest" { };

  stylish-haskell = tool "stylish-haskell" "0.14.4.0" { };

  cabal-fmt = tool "cabal-fmt" "0.1.6" { };

  cabal-docspec = tool "cabal-docspec" "git" {
    src = inputs.cabal-extras;
    cabalProject = ''
      packages: peura cabal-docspec ${inputs.gentle-introduction}
    '';
  };

  scriv = prev.scriv.overrideAttrs (_: {
    version = "1.2.0-custom-iog";
    src = final.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "scriv";
      rev = "567a1aa3f6df77d1a531290f10a261ec6a49c75a";
      hash = "sha256-wpWDuZ3c8JJKVWPw9PHgcpneRWYjd/0z4oAIirPa0/E=";
    };
  });
}
