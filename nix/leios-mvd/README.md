# Leios MVD

Leios Minimum Viable Demo branch with NixOS test based setup for spawning reproducible and/or interactive demo scenarios.

## Files and directories

- `./test.nix` contains the NixOS test definition where we can script various scenarios.
- `./leios-node/` contains the NixOS definition for the "leios-node" that runs the "Ouroboros Leios" patched cardano-node under test.
- `./immdb-node/` contains the NixOS definition for the "immdb-node" that runs the Immutable DB server.

## Running the test

To run the test scenario with Nix:

```shell
$ nix build .#leios-mvd-test
```

Once it finishes the `result` directory will contains the logs collected from the "leios-node":

```shell
$ ls result
cardano-node.logs

$ tail result/cardano-node.logs
Oct 15 14:58:07 leios-node cardano-node-start[741]: {"at":"2025-10-15T14:58:07.199318758Z","ns":"Net.ConnectionManager.Local.ConnectionManagerCounters","data":{"kind":"ConnectionManagerCounters","state":{"duplex":0,"fullDuplex":0,"inbound":1,"outbound":0,"unidirectional":1}},"sev":"Debug","thread":"42","host":"leios-node"}
...
```

## Interactive demo

Fun part! To start the nodes defined by the test scenario run:

```shell
$ nix run .#leios-mvd-test.driverInteractive
additionally exposed symbols:
    immdb-node, leios-node,
    vlan1,
    start_all, test_script, machines, vlans, driver, log, os, create_machine, subtest, run_tests, join_all, retry, serial_stdout_off, serial_stdout_on, polling_condition, Machine
>>>
```

This puts you in a IPython shell. To start all nodes run:

```python
>>> start_all()
```

After a bit you should see QEMU windows corresponding to each node in the test scenario.
Login as "root".
