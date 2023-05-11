let
  actionCiInputName = "GitHub event";
  repository = "input-output-hk/ouroboros-consensus";

  taskDefinitions = [
    { system = "x86_64-linux"; }
    { system = "x86_64-darwin"; }
    { system = "aarch64-darwin"; }
    { system = "x86_64-linux"; attr = "windows"; name = "x86_64-windows"; }
  ];

  mkTask = { system, attr ? "native", ... }: { config, ... }: {
    preset = {
      nix.enable = true;

      github.ci = {
        # Tullia tasks can run locally or on Cicero.
        # When no facts are present we know that we are running locally and vice versa.
        # When running locally, the current directory is already bind-mounted into the container,
        # so we don't need to fetch the source from GitHub and we don't want to report a GitHub status.
        enable = config.actionRun.facts != { };
        inherit repository;
        remote = config.preset.github.lib.readRepository actionCiInputName null;
        revision = config.preset.github.lib.readRevision actionCiInputName null;
      };
    };

    command.text = ''
      nix build -L .#hydraJobs.${system}.required.${attr} \
        --max-silent-time 1800 --keep-going
    '';

    memory = 1024 * 32;

    nomad = {
      resources.cpu = 10000;

      driver = "exec";
    };
  };

  taskCue = ''
    // This is a CUE expression that defines what events trigger a new run of this action.
    // There is no documentation for this yet. Ask SRE if you have trouble changing this.

    let github = {
      #input: "${actionCiInputName}"
      #repo: "${repository}"
    }

    #lib.merge
    #ios: [
      {
        #lib.io.github_push
        github
        #default_branch: true
        #branch: "gh-readonly-queue/.*"
      },
      {
        #lib.io.github_pr
        github
      },
    ]
  '';
in
{
  tasks = builtins.listToAttrs (builtins.map
    (t: {
      name = "ci/${t.name or t.system}";
      value = mkTask t;
    })
    taskDefinitions);

  actions = builtins.listToAttrs (builtins.map
    (t:
      let name = "ci/${t.name or t.system}"; in {
        name = "ouroboros-consensus/${name}";
        value = {
          task = name;
          io = taskCue;
        };
      })
    taskDefinitions);
}
