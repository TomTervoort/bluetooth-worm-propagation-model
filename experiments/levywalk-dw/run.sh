#!/bin/bash

set -e

# Script that executes simulations according to three configuration templates and combines the resulting CSV files.
# Build the CLI (src/build-er.sh) first.

mkdir -p results
for sr in 0.2 0.4 0.6 0.8 1.0; do
    echo '========================'
    echo "unrestricted $sr:"
    cat unrestricted-config-template.json | jq ".simulation.params.infModel.susceptibilityRate = $sr" | time -p ../../bin/exp-runner - | tee results/"unrestricted-$sr.csv"
    echo '------------------------'
    echo "discovery-only $sr:"
    cat discovery-only-config-template.json | jq ".simulation.params.infModel.susceptibilityRate = $sr" | time -p ../../bin/exp-runner - | tee results/"discovery-only-$sr.csv"
    echo '------------------------'
    echo "advanced $sr:"
    cat advanced-config-template.json | jq ".simulation.params.infModel.susceptibilityRate = $sr" | time -p ../../bin/exp-runner - | tee results/"advanced-$sr.csv"
done

cd results
echo 'type,susceptibility,time,mean infection rate,std. dev' > all.csv
find *.csv -name '*-?.?.csv' -exec bash -c 'tail -n +2 {} | sed -E s:^:{},:g' \; | sed -E 's:\.csv::g' | sed -E 's:(unrestricted|discovery-only|advanced)-:\1,:g'  >> all.csv

