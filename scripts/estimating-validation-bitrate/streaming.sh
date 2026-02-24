### Will a full buffer of size B refill before it empties?

out=catch-$B

# The resulting file has 3 columns: SlotNo, CumuMicroseconds, CumuBytes.
tail -n+2 benchmark-ledger-ops.csv | awk -f cumuboth.awk >$out

# Discard Byron.
# cat $out | awk '($1 >= 4492800) { print $0 }' >$out.tmp; mv $out.tmp $out

# Time and space sizes of windows of B-blocks
#
# ChainSel and BlockFetch clients use a buffer of 10 blocks. On top of that,
# BlockFetch itself is buffered according to the low/high watermark, which are
# at least 192 kibibytes and 384 kibibytes, respectively. This logic here only
# considers the block-counted buffer, not the bytes in-flight.
paste $out <(tail -n+$((B + 1)) $out) | awk '(NF > 3) {print $2, $5 - $2, $6 - $3}' >$out.tmp; mv $out.tmp $out

# The scatter plot of this data informs the question: assuming the buffer is
# currently full, what bit rate would be necessary in order to completely
# refill the buffer before it empties.
paste $out <(tail -n+2 $out) | awk '(NF > 3) {print ($1 + $4) / 2, $6 / $2}' >$out.tmp; mv $out.tmp $out
