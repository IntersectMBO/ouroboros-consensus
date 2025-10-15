{ inputs, pkgs, ... }:
{
  name = "Leios MVD NixOS test";

  nodes = {
    immdb-node = {
      imports = [
        (import ./immdb-node/os.nix {
          immdb-server = pkgs.hsPkgs.ouroboros-consensus-cardano.getComponent "exe:immdb-server";
        })
      ];
      networking.domain = "local";
    };
    leios-node = {
      imports = [
        (import ./leios-node/os.nix { inherit inputs; })
      ];
      networking.domain = "local";
      environment.systemPackages = [ pkgs.dnsutils ];
      services.resolved.enable = true;
    };
  };

  testScript = ''
    start_all()

    immdb_node.wait_for_unit("immdb-server.service")
    leios_node.wait_for_unit("cardano-node.service")
  '';
}
