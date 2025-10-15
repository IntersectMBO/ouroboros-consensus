{immdb-server, ...}:{
  imports = [
    (import ./service.nix {inherit immdb-server;})
  ];

  cardano.immdb-server.enable = true;
}
