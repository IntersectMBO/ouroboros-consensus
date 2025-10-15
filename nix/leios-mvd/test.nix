{inputs, pkgs, ...}:
{
  name = "Leios MVD NixOS test";

  nodes = {
    immdb-node = import ./immdb-node/os.nix {
      immdb-server = pkgs.hsPkgs.ouroboros-consensus-cardano.getComponent "exe:immdb-server";
    };
    leios-node = import ./leios-node/os.nix {inherit inputs;};
  };

  testScript = ''
    start_all()

    immdb_node.wait_for_unit("immdb-server.service")
    leios_node.wait_for_unit("cardano-node.service")
    '';
}
