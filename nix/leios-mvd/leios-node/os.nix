{ inputs, ... }: {
  imports = [
    inputs.cardano-nix.nixosModules.default
  ];
  cardano = {
    networkNumber = 42;
    private-testnet-node = {
      enable = true;
      nodeConfigFile = ./config.json;

      genesisAlonzo = ../genesis/genesis.alonzo.json;
      genesisConway = ../genesis/genesis.conway.json;
      genesisShelley = ../genesis/genesis.shelley.json;
      genesisByron = ../genesis/genesis.byron.json;

      topology = ./topology.json;

      vrfKey = ./vrf.skey;
      kesKey = ./kes.skey;
      operationalCertificate = ./opcert;
      delegationCertificate = ./delegation-cert.json;
      signingKey = ./delegate.key;
    };
  };
}
