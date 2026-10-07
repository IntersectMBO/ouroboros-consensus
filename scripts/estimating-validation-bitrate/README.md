- `db-analyser --benchmark-ledger-ops` lists the fixed stats, including validation times and the full blockSize.

- `MsgBlock` has an overhead of 2 bytes (ie the list length and the word tag, since both are <24).

- I _think_ `network-mux`'s SDU overhead is 8 bytes, from https://github.com/IntersectMBO/ouroboros-network/blob/db61131c2f375842f0930a8a9cf7f83b0cb80992/network-mux/src/Network/Mux/Codec.hs#L28-L40.
  However, I don't know how many SDUs each byte requires.
  So I'll omit this.
 
Thus the number of bytes on-the-wire is sufficiently dominated by blockSize.

-----

The `nix-shell -p gawk gnuplot --run 'source stream-then-plot.sh'` command renders images that help roughly answer the question: will a full buffer containing B blocks be able to refill before its entire contents is validated?
(As of slot 134028831, it runs for about about 3.5 minutes on my laptop.)

The image width is as great as Cairo would allow.
If you open them in Firefox and then left-click, it will zoom to full height; then you can scroll along the x-axis.
