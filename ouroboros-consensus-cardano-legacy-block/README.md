# Cardano legacy blocks

In UTxO-HD, parts of the ledger state will be stored on-disk. As a result, parts
of the consensus interface change: we need to be explicitly carry around ledger,
which represent the parts of the ledger state that should be stored on the disk.
However, some downstream packages only depend on consensus for the definitions
of block (re-)application, and these packages can ignore ledger tables. The
`ouroboros-consensus-cardano-legacy-block` package defines an alternative
`CardanoBlock`, called `LegacyCardanoBlock`, which provides block
(re-)application that ignores ledger tables. Through some type manipulation and
casting, `CardanoBlock`s can be converted to `LegacyCardanoBlock`s and vice
versa. For an example, see the `DBSync` module.