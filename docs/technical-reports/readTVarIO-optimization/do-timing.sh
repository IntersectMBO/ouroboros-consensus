(
set -eu
ghc -O --make -main-is "$1" "$1.hs" -rtsopts 2>&1 -fforce-recomp
for i in $(seq 30); do
    for w in U O; do
        "./$1" "$w" +RTS -s 2>&1
    done
done | grep -e 'MUT     time'
)
