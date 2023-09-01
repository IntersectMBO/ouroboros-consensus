{pkgs, self}: let
  local = self.legacyPackages.${pkgs.system}.long-range-attack;

  common = ''
  root=''${root:-$PWD}
  base="$root/long-range-attack"
  state="$base/testnet"

  log() {
    echo -e "\e[1m\e[35m>>>\e[0m $*"
  }

  tmuxw() {
    cmd=$1
    shift
    tmux "$cmd" -e root="$root" -P -F '#{pane_id}' "$@"
  }

  quit() {
    kill %1 &>/dev/null || true
  }

  trap 'quit' EXIT
  '';

  scriptWith = runtimeInputs: name: text: let
    exe = "poc-${name}";
    path = pkgs.writeShellApplication {
      inherit runtimeInputs;
      name = exe;
      checkPhase = "";
      text = ''
      ${common}
      ${text}
      '';
    };
  in "${path}/bin/${exe}";

  script = scriptWith [];

  bootstrap = script "bootstrap" ''
  if ! [[ -f $state/config.yaml ]]
  then
    log 'Extracting testnet data.'
    mkdir -p "$state"
    tar -xf "$base/testnet.tar.xz" -C "$base"
  fi
  '';

  pane1 = script "pane1" ''
  log 'Starting honest node'
  $state/start-honest.sh ${local.baseline}/bin/cardano-node
  '';

  pane2 = script "pane2" ''
  log 'Starting adversarial node'
  $state/start-adversary.sh ${local.baseline}/bin/cardano-node
  '';

  pane4 = script "pane4" ''
  log 'Starting toxiproxy server'
  ${pkgs.toxiproxy}/bin/toxiproxy-server
  '';

  pane5 = scriptWith [pkgs.toxiproxy] "pane5" ''
  log 'Configuring toxiproxy connections and setting latencies'
  $state/toxiproxy.sh
  log 'Running tip watcher'
  ${pkgs.procps}/bin/watch --color -n1 $state/query-syncing.sh ${local.cardano-cli}/bin/cardano-cli
  '';

  pane3 = script "pane3" ''
  log 'Waiting for a syncing node to be started. Please press [g] or [n] in the prompt window.'
  '';

  genesis = script "genesis" ''
  $state/start-syncing.sh ${local.genesis-poc}/bin/cardano-node
  '';

  nonGenesis = script "non-genesis" ''
  $state/start-syncing.sh ${local.baseline}/bin/cardano-node
  '';

  ui = script "ui" ''
  running=true
  kill=true

  exit_hook() {
    if [[ $kill == true ]]
    then
      tmux kill-window -t genesis
    fi
  }
  trap exit_hook EXIT

  log 'Select an action:'
  echo -e ' [\e[34mg\e[0m] to start a syncing Genesis node'
  echo -e ' [\e[34mn\e[0m] to start a syncing non-Genesis node'
  echo -e ' [\e[34mq\e[0m] to kill the processes and close the window'
  echo -e ' [\e[34mx\e[0m] to terminate this prompt and leave the processes running'
  echo -en ' \e[1m\e[33m>\e[0m '

  while [[ $running == true ]]
  do
    read -s -n1 action
    case $action in
      q)
        running=false
        ;;
      x)
        kill=false
        running=false
        ;;
      g)
        tmux send-keys -t $pane C-c
        tmux send-keys -t $pane ${genesis} ENTER
        ;;
      n)
        tmux send-keys -t $pane C-c
        tmux send-keys -t $pane ${nonGenesis} ENTER
        ;;
    esac
  done
  '';

  setupPanes = script "setup-panes" ''
  pane1=$(tmuxw new-window -k -n genesis ${pane1})
  tmux set-option -w -t $pane1 remain-on-exit on
  pane3=$(tmuxw split-window -t $pane1 -v)
  tmux send-keys -t $pane3 ${pane3} ENTER
  pane_ui=$(tmuxw split-window -t $pane3 -v -l 20% -e pane=$pane3 ${ui})
  pane4=$(tmuxw split-window -t $pane3 -h ${pane4})
  pane5=$(tmuxw split-window -t $pane4 -h ${pane5})
  pane2=$(tmuxw split-window -t $pane1 -h ${pane2})
  tmux select-pane -t $pane_ui
  '';

  tmuxServer = scriptWith [pkgs.tmux] "tmux-server" ''
  ${pkgs.tmux}/bin/tmux new-session -s genesis -n boot ${setupPanes}
  '';

  run = script "run" ''
  ${bootstrap}
  if [[ -n ''${TMUX-} ]]
  then
    log 'Using active tmux session'
    ${setupPanes}
  else
    log 'Starting tmux server'
    ${tmuxServer}
  fi
  '';

in {
  inherit bootstrap run;
}
