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

    # Wait until the respective services are up
    immdb_node.wait_for_unit("immdb-server.service")
    leios_node.wait_for_unit("cardano-node.service")

    # Wait until leios-node synced with immdb-node
    # NOTE(bladyjoker): Block 51 is the tip
    # [0.139717s] BlockNo 51  SlotNo 994      6685f44f32433d0817b6edf5f9e00aaaa3c4986524b8b453a620825747a936cc
    leios_node.wait_until_succeeds("cardano-cli query tip | grep hash | grep -q '6685f4'")

    # Collect logs from leios-node (read them in result/cardano.logs)
    leios_node.execute("journalctl -u cardano-node --no-pager > cardano-node.logs")
    leios_node.copy_from_vm("cardano-node.logs", "")
  '';
}
