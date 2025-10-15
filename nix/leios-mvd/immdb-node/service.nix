{ immdb-server, ... }:
{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.cardano.immdb-server;
in
{

  imports = [
  ];

  options.cardano.immdb-server = {
    enable = lib.mkEnableOption ''Run ourborous-consensus's immdb-server as a service'';

    db = lib.mkOption {
      type = lib.types.path;
      description = "Path to the ImmutableDB";
      default = ./immutable;
    };

    config = lib.mkOption {
      type = lib.types.path;
      description = "Path to config file, in the same format as for the node or db-analyser";
      default = ./config.json;
    };

    genesisAlonzo = lib.mkOption {
      type = lib.types.path;
      default = ../genesis/genesis.alonzo.json;
    };

    genesisConway = lib.mkOption {
      type = lib.types.path;
      default = ../genesis/genesis.conway.json;
    };

    genesisByron = lib.mkOption {
      type = lib.types.path;
      default = ../genesis/genesis.byron.json;
    };

    genesisShelley = lib.mkOption {
      type = lib.types.path;
      default = ../genesis/genesis.shelley.json;
    };

    address = lib.mkOption {
      type = lib.types.str;
      description = "Address to serve on";
      default = "0.0.0.0";
    };

    port = lib.mkOption {
      type = lib.types.port;
      description = "Port to serve on";
      default = 3001;
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "immdb-server";
      description = ''User to run immdb-server service as.'';
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "immdb-server";
      description = ''Group to run immdb-server service as.'';
    };

  };

  config = lib.mkIf cfg.enable {
    users = {
      users = {
        ${cfg.user} = {
          description = "User to run immdb-server service";
          inherit (cfg) group;
          createHome = false;
          isSystemUser = true;
        };
      };

      groups = {
        ${cfg.group} = {
          members = [ cfg.user ];
        };
      };
    };

    networking.firewall.allowedTCPPorts = [ cfg.port ];

    environment.etc = {
      "immdb-server/config.json" = {
        user = cfg.user;
        group = cfg.group;
        source = cfg.config;
      };
      "immdb-server/genesis-alonzo.json" = {
        user = cfg.user;
        group = cfg.group;
        source = cfg.genesisAlonzo;
      };
      "immdb-server/genesis-conway.json" = {
        user = cfg.user;
        group = cfg.group;
        source = cfg.genesisConway;
      };
      "immdb-server/genesis-byron.json" = {
        user = cfg.user;
        group = cfg.group;
        source = cfg.genesisByron;
      };
      "immdb-server/genesis-shelley.json" = {
        user = cfg.user;
        group = cfg.group;
        source = cfg.genesisShelley;
      };

    };

    systemd.services.immdb-server = {
      enable = true;
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "on-failure";
        RemainAfterExit = true;
        User = "immdb-server";
        Group = "immdb-server";
        ConfigurationDirectory = [ "immdb-server" ];
        StateDirectory = [ "immdb-server" ];
      };

      path = [ immdb-server ];

      script = ''
        echo "Starting Immutable DB server with ${builtins.toJSON cfg}";

        mkdir $STATE_DIRECTORY/immutable;
        cp -r ${cfg.db}/* $STATE_DIRECTORY/immutable;

        immdb-server \
          --db $STATE_DIRECTORY/immutable \
          --config $CONFIGURATION_DIRECTORY/config.json \
          --address ${cfg.address} \
          --port ${builtins.toString cfg.port};
      '';
    };

  };

}
