export CDDL_INCLUDE_PATH=ledger-cddls/eras/byron/cddl-spec:ledger-cddls/eras/shelley/impl/cddl-files:ledger-cddls/eras/allegra/impl/cddl-files:ledger-cddls/eras/mary/impl/cddl-files:ledger-cddls/eras/alonzo/impl/cddl-files:ledger-cddls/eras/babbage/impl/cddl-files:ledger-cddls/eras/conway/impl/cddl-files:disk:.:

mkdir -p out

gen () {
    echo -n -e "\t- $1"
    mkdir -p out/$(echo $1 | rev | cut -d'/' -f2- | rev)
    cddlc -u2tcddl $1.cddl > "out/$1.compiled.cddl"
    echo " ok"
}

IN=$(fd -e cddl -E ledger-cddls -E out -E base.cddl -E node-to-client/localstatequery/query.cddl -E node-to-client/localstatequery/result.cddl)

echo "Generating complete CDDLs:"
for f in $IN; do
    gen $(echo $f | cut -d'.' -f1)
done

UNDEFINEDS=$(grep -R undefined out)
if [ ! -z "$UNDEFINEDS" ]; then
    echo -e "\033[0;31mThere were undefined references!\n$UNDEFINEDS\033[0m"

    while IFS= read -r line || [[ -n $line ]]; do
        fileName=$(echo $line | cut -d':' -f1)
        ref=$(echo $line | cut -d':' -f3 | tr -d ' ')
        echo "$ref = any" >> $fileName
    done < <(printf '%s' "$UNDEFINEDS")
fi

echo "Done"
