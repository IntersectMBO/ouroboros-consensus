inputs: final: prev:

let
  tool = name: version: other:
    final.haskell-nix.tool final.hsPkgs.args.compiler-nix-name name ({
      version = version;
      index-state = "2023-04-20T00:00:00Z";
    } // other);
in
{
  cabal = tool "cabal" "latest" { };

  stylish-haskell = tool "stylish-haskell" "0.14.4.0" {
    cabalProjectLocal = "allow-older: ghc-lib-parser:base";
  };

  cabal-fmt = tool "cabal-fmt" "0.1.6" { };

  haskell-language-server = tool "haskell-language-server" "1.10.0.0" { };

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
