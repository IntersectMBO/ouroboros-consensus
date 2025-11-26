I intend to update this file tomorrow morning before the demo, but I want at
least a stub for now, since I already put the link on the slides.

If you need to see some details right now, read the raw comments at
https://github.com/IntersectMBO/ouroboros-consensus/issues/1756 to see our
learning path.

If you want to build it, you'll need to build the packages in these five
repositories on these commits. Be advised: they do not already have
`source-repository-package` stanzas in their `cabal.project` files, if that's
your usual method.

```
$ (cd ..; for i in ouroboros-consensus ouroboros-network ekg-forward cardano-node cardano-api; do (cd $i; echo $i; git lg -2); done)
ouroboros-consensus
32fe38674 - (HEAD -> nfrisby/leios-202511-demo, origin/nfrisby/leios-202511-demo) leiosdemo202511: stub out README-Leios-November-demo.md (24 seconds ago) <Nicolas Frisby>
c85e8bf7a - leiosdemo202511: log_parser.py for ChainSync and BlockFetch too (33 minutes ago) <Nicolas Frisby>
ouroboros-network
ebd307e676 - (HEAD -> nfrisby/leios-202511-demo) Revert "leiosdemo202511: set TCP_NOTSENT_LOWAT 16384 on all sockets" (27 minutes ago) <Nicolas Frisby>
df052fac8f - (origin/nfrisby/leios-202511-demo, nfrisby/issue-consensus-1756) leiosdemo202511: another bugfix in runDecoder* (5 hours ago) <Nicolas Frisby>
ekg-forward
81e671e - (HEAD -> nfrisby/leios-202511-demo, origin/nfrisby/leios-202511-demo) leiosdemo202511: fixup build for ouroboros-network:runDriver* changes (4 hours ago) <Nicolas Frisby>
06a66f7 - leiosdemo202511: bump o-n-f upperbound (7 hours ago) <Nicolas Frisby>
cardano-node
0aaa2a12ce - (HEAD -> nfrisby/leios-202511-demo, origin/nfrisby/leios-202511-demo, nfrisby/leiosdemo2025) leiosdemo202511: hacky special case for mux_at (4 hours ago) <Nicolas Frisby>
9e457f93aa - leiosdemo202511: fixup build for ouroboros-network:runDriver* and TraceSendRecv changes (6 hours ago) <Nicolas Frisby>
cardano-api
762984a5c - (HEAD -> nfrisby/leios-202511-demo, origin/nfrisby/leios-202511-demo) leiosdemo202511: fixup build for ouroboros-network:runDriver* changes (7 hours ago) <Nicolas Frisby>
6c6025361 - (tag: cardano-api-10.16.3.0) Release cardano-api-10.16.3.0 (6 months ago) <Pablo Lamela>
```
