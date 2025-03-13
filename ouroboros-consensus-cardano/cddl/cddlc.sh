mkdir -p out

gen () {
    echo -n -e "\t- $1"
    mkdir -p out/$(echo $1 | rev | cut -d'/' -f2- | rev)
    cddlc -u2tcddl $1.cddl > "out/$1.compiled.cddl"
    echo " ok"
}

CDDL_INCLUDE_PATH=""
for f in $(fd -t d); do
    CDDL_INCLUDE_PATH+="$f:"
done

export CDDL_INCLUDE_PATH=$CDDL_INCLUDE_PATH.:

IN=$(fd -e cddl \
        -E ledger-cddls \
        -E out \
        -E base.cddl \
        -E node-to-client/localstatequery/shelley \
        -E node-to-client/localstatequery/byron \
        -E node-to-client/localstatequery/consensus \
        -E disk/snapshot
  )

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
