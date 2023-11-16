(
set -eu
ghc -O --make -main-is "$1" "$1.hs" -rtsopts -fforce-recomp -ticky -ddump-simpl -dsuppress-all -dno-typeable-binds 2>&1 | cat >catch-compile.txt
for w in U O; do
    "./$1" "$w" +RTS -A2.5G -s -r"${w}.ticky" 2>"${w}.rts"
done
diff <(cat U.{ticky,rts}) <(cat O.{ticky,rts}) | tee catch-diff.txt
)
