{inputs, pkgs, ...}:
{
  name = "Leios MVD NixOS test";

  nodes = {
    leios-node = import ./leios-node/os.nix {inherit inputs;};
  };

  testScript = ''
    start_all()

    leios_node.wait_for_unit("cardano-node.service")
    '';
}
