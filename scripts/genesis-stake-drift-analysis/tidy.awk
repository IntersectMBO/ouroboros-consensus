/EpochNo/ {
    eno = $3;
    n = $7;
    i = 0;
}

(/%/) {
    print eno, i, n, $1, $5 / $7;
    i++;
}
