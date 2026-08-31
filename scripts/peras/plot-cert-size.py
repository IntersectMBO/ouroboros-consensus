import csv
import sys

import matplotlib.pyplot as pyplot

EXPECTED_FIELDS = {"non_persistent_voters", "size"}

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
        (int(row["non_persistent_voters"]), int(row["size"])) for row in rows
    ]
except (KeyError, TypeError, ValueError):
    print("input CSV has invalid numeric data", file=sys.stderr)
    sys.exit(1)

if not measurements:
    print("input CSV contains no measurements", file=sys.stderr)
    sys.exit(1)

if any(size <= 0 for _, size in measurements):
    print("input CSV has non-positive sizes", file=sys.stderr)
    sys.exit(1)

measurements.sort()
non_persistent_voters = [measurement[0] for measurement in measurements]
serialized_bytes = [measurement[1] for measurement in measurements]

pyplot.rcParams["svg.hashsalt"] = "peras-cert-size"
pyplot.figure(figsize=(8, 5))
pyplot.plot(non_persistent_voters, serialized_bytes)
pyplot.grid()
pyplot.xlabel("Non-persistent voters")
pyplot.ylabel("Serialized V1.PerasCert size (bytes)")
pyplot.title("V1.PerasCert serialized size")
pyplot.tight_layout()
pyplot.savefig(sys.argv[2], metadata={"Date": None})
