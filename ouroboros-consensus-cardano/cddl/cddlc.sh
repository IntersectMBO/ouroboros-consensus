export CDDL_INCLUDE_PATH=ledger-cddls/eras/byron/cddl-spec:ledger-cddls/eras/shelley/impl/cddl-files:ledger-cddls/eras/allegra/impl/cddl-files:ledger-cddls/eras/mary/impl/cddl-files:ledger-cddls/eras/alonzo/impl/cddl-files:ledger-cddls/eras/babbage/impl/cddl-files:ledger-cddls/eras/conway/impl/cddl-files:disk:.:

mkdir -p out

gen () {
    echo -n -e "\t- $1"
    mkdir -p out/$(echo $1 | rev | cut -d'/' -f2- | rev)
    cddlc -u2tcddl $1.cddl > "out/$1.compiled.cddl"
    echo " ok"
}

IN=$(fd -e cddl -E ledger-cddls -E out -E base.cddl)

echo "Generating complete CDDLs:"
for f in $IN; do
    gen $(echo $f | cut -d'.' -f1)
done
