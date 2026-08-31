import csv
import sys

import matplotlib.pyplot as pyplot

EXPECTED_FIELDS = {"persistent_voters", "non_persistent_voters", "size"}

if len(sys.argv) != 3:
    print("usage: plot-cert-size.py INPUT.csv OUTPUT.svg", file=sys.stderr)
    sys.exit(1)

with open(sys.argv[1], newline="", encoding="utf-8") as input_file:
    reader = csv.DictReader(input_file)
    if set(reader.fieldnames or []) != EXPECTED_FIELDS:
        print("input CSV has unexpected columns", file=sys.stderr)
        sys.exit(1)
    rows = list(reader)

try:
    measurements = [
        (
            int(row["persistent_voters"]),
            int(row["non_persistent_voters"]),
            int(row["size"]),
        )
        for row in rows
    ]
except (KeyError, TypeError, ValueError):
    print("input CSV has invalid numeric data", file=sys.stderr)
    sys.exit(1)

if not measurements:
    print("input CSV contains no measurements", file=sys.stderr)
    sys.exit(1)

if any(size <= 0 for _, _, size in measurements):
    print("input CSV has non-positive sizes", file=sys.stderr)
    sys.exit(1)

measurements.sort()
persistent_voter_counts = sorted({measurement[0] for measurement in measurements})

pyplot.rcParams["svg.hashsalt"] = "peras-cert-size"
pyplot.figure(figsize=(8, 5))
for persistent_voters in persistent_voter_counts:
    series = [
        (non_persistent_voters, size)
        for persistent, non_persistent_voters, size in measurements
        if persistent == persistent_voters
    ]
    non_persistent_voters = [measurement[0] for measurement in series]
    serialized_bytes = [measurement[1] for measurement in series]
    pyplot.plot(
        non_persistent_voters,
        serialized_bytes,
        label=f"{persistent_voters} persistent voters",
    )
pyplot.grid()
pyplot.xlabel("Non-persistent voters")
pyplot.ylabel("Serialized V1.PerasCert size (bytes)")
pyplot.title("V1.PerasCert serialized size")
pyplot.legend(title="Persistent voters")
pyplot.tight_layout()
pyplot.savefig(sys.argv[2], metadata={"Date": None})
