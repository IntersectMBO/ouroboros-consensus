{ immdb-server, ... }: {
  imports = [
    (import ./service.nix { inherit immdb-server; })
  ];

  cardano.immdb-server.enable = true;
  cardano.immdb-server.initialSlot = 500;
}
