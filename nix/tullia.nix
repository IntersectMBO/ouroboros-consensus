let
  actionCiInputName = "GitHub event";
in
{
  tasks = {
    ci = { config, lib, ... }: {
      preset = {
        nix.enable = true;

        github.ci = {
          # Tullia tasks can run locally or on Cicero.
          # When no facts are present we know that we are running locally and vice versa.
          # When running locally, the current directory is already bind-mounted into the container,
          # so we don't need to fetch the source from GitHub and we don't want to report a GitHub status.
          enable = config.actionRun.facts != { };
          repository = "input-output-hk/ouroboros-consensus";
          remote = config.preset.github.lib.readRepository actionCiInputName null;
          revision = config.preset.github.lib.readRevision actionCiInputName null;
        };
      };

      command.text = config.preset.github.status.lib.reportBulk {
        bulk.text = ''
          nix eval .#hydraJobs --apply __attrNames --json |
          nix-systems -i |
          jq 'with_entries(select(.value))' # filter out systems that we cannot build for
        '';
        each.text =
          ''nix build -L .#hydraJobs."$1".required --max-silent-time 1800 --keep-going'';
        skippedDescription =
          lib.escapeShellArg "No nix builder available for this system";
      };

      memory = 1024 * 32;

      nomad = {
        resources.cpu = 10000;

        driver = "exec";
      };
    };
  };

  actions = {
    "ouroboros-consensus/ci" = {
      task = "ci";
      io = ''
        // This is a CUE expression that defines what events trigger a new run of this action.
        // There is no documentation for this yet. Ask SRE if you have trouble changing this.

        let github = {
          #input: "${actionCiInputName}"
          #repo: "input-output-hk/ouroboros-consensus"
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
    };
  };
}
