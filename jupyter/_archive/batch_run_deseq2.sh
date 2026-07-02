#!/usr/bin/env bash
set -euo pipefail

COUNTS=data/count_matrix.csv.gz
METADIR=data/sample_metadata
DEGDIR=data/DEG
QC=${METADIR}/meta/sample_metadata_by_virus_with_tpm.tsv

MIN_POS=3     # require at least 3 positives
MIN_REST=3    # and at least 3 in Rest

mkdir -p "$DEGDIR"

# Get total cohort size from any per-virus TSV (all have same rows)
ANY_TSV=$(ls ${METADIR}/sample_metadata_*.tsv | grep -v 'sample_metadata_by_virus_min.tsv' | head -n1)
TOTAL=$(( $(wc -l < "$ANY_TSV") - 1 ))
echo "[i] Cohort size: $TOTAL strains"

# Build virus list with enough positives and enough rest
# QC file has only positive rows; count positives per virus (Subpop column).
awk -F'\t' 'NR>1 {pos[$2]++} END {for (v in pos) print v, pos[v]}' "$QC" \
| awk -v TOTAL="$TOTAL" -v MIN_POS="$MIN_POS" -v MIN_REST="$MIN_REST" '
  {
    virus=$1; pos=$2; rest=TOTAL-pos;
    if (pos>=MIN_POS && rest>=MIN_REST) print virus;
  }' \
| sort > /tmp/virus_allowlist.txt

echo "[i] Viruses to run: $(wc -l < /tmp/virus_allowlist.txt)"

# Run sequentially (simple & readable). Remove --halt so one failure doesn’t kill the batch.
while read -r virus; do
  tsv="${METADIR}/sample_metadata_${virus}.tsv"
  if [[ ! -f "$tsv" ]]; then
    echo "[warn] Missing TSV for $virus → ${tsv} (skipping)"; continue
  fi
  echo ">>> $virus"
  if ! Rscript run_deseq2.R "$COUNTS" "$tsv" "$virus" "$DEGDIR"; then
    echo "[fail] $virus (continuing)"; continue
  fi
done < /tmp/virus_allowlist.txt
