# please run these first
#
# (cd datadir; for i in pop pop-write write write-try try-pipe pipe pipe-valid valid-select select epilogue between ChainSel; do cat durations.txt | grep -E -e "^END ${i}@" | awk '{print $3}' >"ts_${i}.txt"; cat "ts_${i}.txt" | sort -n >"edf_${i}.txt"; done)
#
# join <(cat datadir/durations.txt | grep -E -e "^END pipe-valid@" | tr @ ' ' | awk '{print $3, int($4 / 1000)}' | sort -k 1b,1)  <(cat ../full-2024-09-20.csv | awk '(1 < NR && $14 < 600000) {print $1, $10 + $11 + $12 + $13}' | sort -k 1b,1 ) | sort -n >combo.txt
#
# FORMERLY:
#
# (cd datadir; for i in pop pop-write write write-try try-pipe pipe pipe-valid valid-select select epilogue between ChainSel; do cat elab.totals.txt | grep -E -e "^${i}@" | tr @ ' ' | awk '{print $3}' | grep -v -w -e init | tr -d ns | tac >"edf_${i}.txt"; cat elab.totals.txt | grep -E -e "^${i}@" | tr @ ' ' | awk '{print $2, $3}' | grep -v -w -e init | sort -n | tr -d ns | awk '{print $2}' >"ts_${i}.txt"; done)
#
# join <(cat datadir/elab.totals.txt | grep -E -e "^pipe-valid@" | tr @ ' ' | awk '{print $2, $3}' | grep -v -w -e init | sort -n | tr -d ns | awk '{print $1, int($2 / 1000)}' | sort -k 1b,1)  <(cat ../benchmark-ledger-ops.csv | awk '(1 < NR && $14 < 600000) {print $1, $10 + $11 + $12 + $13}' | sort -k 1b,1 )   | sort -n >combo.txt


set terminal pngcairo transparent enhanced size 1024,512 font "sans,8"

stats 'datadir/edf_ChainSel.txt'
N = floor(STATS_records)

xxx    = 99
cutoff = (xxx / 100.0) * N

set output 'edf.png'
set title 'empirical cdfs up to ' . sprintf('%d', xxx) . '%, N=' . sprintf('%d', N)

set key top left

set logscale y
set grid

set ylabel 'nanoseconds'
set xlabel 'proportion'

f(x) = x > cutoff ? 1/0 : x / N

plot \
  'datadir/edf_pop.txt' using (f(column(0))):1:(0.1)               title 'pop' with points pointtype 7 pointsize variable,\
  'datadir/edf_pop-write.txt' using (f(column(0))):1:(0.1)         title 'pop-write' with points pointtype 7 pointsize variable,\
  'datadir/edf_write.txt' using (f(column(0))):1:(0.1)             title 'write' with points pointtype 7 pointsize variable,\
  'datadir/edf_write-try.txt' using (f(column(0))):1:(0.1)         title 'write-try' with points pointtype 7 pointsize variable,\
  'datadir/edf_try-pipe.txt' using (f(column(0))):1:(0.1)          title 'try-pipe' with points pointtype 7 pointsize variable,\
  'datadir/edf_pipe.txt' using (f(column(0))):1:(0.1)              title 'pipe' with points pointtype 7 pointsize variable,\
  'datadir/edf_pipe-valid.txt' using (f(column(0))):1:(0.1)        title 'pipe-valid' with points pointtype 7 pointsize variable,\
  'datadir/edf_valid-select.txt' using (f(column(0))):1:(0.1)      title 'valid-select' with points pointtype 7 pointsize variable,\
  'datadir/edf_select.txt' using (f(column(0))):1:(0.1)            title 'select' with points pointtype 7 pointsize variable,\
  'datadir/edf_epilogue.txt' using (f(column(0))):1:(0.1)          title 'epilogue' with points pointtype 7 pointsize variable,\
  'datadir/edf_ChainSel.txt' using (f(column(0))):1:(0.1)          title 'ChainSel' with points pointtype 7 pointsize variable,\
  'datadir/edf_between.txt' using (f(column(0))):(0.01 * $1):(0.1) title '0.01 * between' with points pointtype 7 pointsize variable,\

set output 'ts.png'
set title 'time series, N=' . sprintf('%d', N)
set xlabel 'normalized sample index'

g(x) = x / N

plot \
  'datadir/ts_pop.txt' using (g(column(0))):1:(0.1)               title 'pop' with points pointtype 7 pointsize variable,\
  'datadir/ts_pop-write.txt' using (g(column(0))):1:(0.1)         title 'pop-write' with points pointtype 7 pointsize variable,\
  'datadir/ts_write.txt' using (g(column(0))):1:(0.1)             title 'write' with points pointtype 7 pointsize variable,\
  'datadir/ts_write-try.txt' using (g(column(0))):1:(0.1)         title 'write-try' with points pointtype 7 pointsize variable,\
  'datadir/ts_try-pipe.txt' using (g(column(0))):1:(0.1)          title 'try-pipe' with points pointtype 7 pointsize variable,\
  'datadir/ts_pipe.txt' using (g(column(0))):1:(0.1)              title 'pipe' with points pointtype 7 pointsize variable,\
  'datadir/ts_pipe-valid.txt' using (g(column(0))):1:(0.1)        title 'pipe-valid' with points pointtype 7 pointsize variable,\
  'datadir/ts_valid-select.txt' using (g(column(0))):1:(0.1)      title 'valid-select' with points pointtype 7 pointsize variable,\
  'datadir/ts_select.txt' using (g(column(0))):1:(0.1)            title 'select' with points pointtype 7 pointsize variable,\
  'datadir/ts_epilogue.txt' using (g(column(0))):1:(0.1)          title 'epilogue' with points pointtype 7 pointsize variable,\
  'datadir/ts_ChainSel.txt' using (g(column(0))):1:(0.1)          title 'ChainSel' with points pointtype 7 pointsize variable,\
  'datadir/ts_between.txt' using (g(column(0))):(0.01 * $1):(0.1) title '0.01 * between' with points pointtype 7 pointsize variable,\

set output 'sanity.png'
#set title 'min 1 ((benchmark-ledger-ops - pipe-valid) / pipe-valid), i7-1165G7 2.80GHz/32GB, N=' . sprintf('%d', N)
#set ylabel 'ratio'
set xlabel "slot's onset - slot 0's onset (s)"

unset logscale y

onset(x) = x < 4492800 ? x*20 : 4492800*20 + (x - 4492800)

clamp1(x) = x < 1 ? x : 1

#plot 'combo.txt' using (onset($1)):(clamp1(($3 - $2) / $2)):(0.1) pointsize variable notitle

#plot \
#  'combo.txt' using (onset($1)):2:(0.1) pointsize variable notitle,\
#  'combo.txt' using (onset($1)):3:(0.1) pointsize variable notitle,\
#  'combo.txt' using (onset($1)):(abs($2-$3)):(0.1) pointsize variable title 'diff',\

cumu2 = 0
cumu3 = 0
r100  = 95
r     = r100 / 100.0

set title 'exponential smoothing alpha=0.' . sprintf('%d', r100) . ', i7-1165G7 2.80GHz/32GB, N=' . sprintf('%d', N)
set ylabel 'microseconds'

cumu23 = 0
cumu2b = 0
cumu3b = 0

#set xrange [1.3e8:1.9e8]
#set yrange [0:70000]

plot \
  'combo.txt' using (onset($1)):(cumu2=cumu2*r + (1-r)*$2):(0.1) pointtype 7 pointsize variable title 'pipe-valid',\
  'combo.txt' using (onset($1)):(cumu3=cumu3*r + (1-r)*$3):(0.1) pointtype 7 pointsize variable title '--benchmark-ledger-ops (2024 Sept 20)',\
  'combo.txt' using (onset($1)):(cumu2b=cumu2b*r + (1-r)*$2, cumu3b=cumu3b*r + (1-r)*$3, abs(cumu3b - cumu2b)):(0.1) pointtype 7 pointsize variable title 'abs diff',\
  'combo.txt' using (onset($1)):(cumu23=cumu23*r + (1-r)*abs($3 - $2)):(0.1) pointtype 7 pointsize variable title 'abs diff 2',\

# ChainSel = forever $
#            epilogue XOR between
#        TraceAddBlockEvent (PoppedBlockFromQueue RisingEdge)
#            pop (wait until queue is non-empty)
#        TraceAddBlockEvent (PoppedBlockFromQueue (FallingEdgeWith rp))
#            pop-write (chainSelSync's init and classification)
#        TraceAddBlockEvent (AddedBlockToVolatileDB _p _bno _ Enclose.RisingEdge)
#            write
#        TraceAddBlockEvent (AddedBlockToVolatileDB _p _bno _ Enclose.FallingEdge)
#            write-try (chainSelectionForBlock's init and classification)
#        TraceAddBlockEvent TryAddToCurrentChain{}
#            try-pipe (identify candidates, build ChainDiffs, sort candidates)
#        TraceAddBlockEvent (PipeliningEvent (SetTentativeHeader _hdr Enclose.RisingEdge)
#            pipe (check/set tentative header)
#        TraceAddBlockEvent (PipeliningEvent (SetTentativeHeader _hdr Enclose.FallingEdge)
#            valid (call ledger)
#        TraceAddBlockEvent ValidCandidate{}
#            valid-select (build ValidatedChainDiff)
#        TraceAddBlockEvent ChangingSelection{}
#            select (init switchTo, update cdbChain, LedgerDB, and varTentativeHeader)
#        TraceAddBlockEvent AddedToCurrentChain{}
#            epilogue (signal delivery, wait to run again)
#
#    the ChainSel interval is start of pop to end of epilogue (ie everything except between)


set output 'corr.png'

set title 'Correlation' . ', i7-1165G7 2.80GHz/32GB, N=' . sprintf('%d', N)
set logscale y
set logscale x
set xlabel 'pipe-valid'
set ylabel '--benchmark-ledger-ops (2024 Sept 20)'

unset xrange
unset yrange

f(x) = m*x + b

fit f(x) 'combo.txt' using 2:3 via m,b

plot 'combo.txt' using 2:3 notitle, f(x) with lines notitle
