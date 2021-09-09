#!/usr/bin/env bash

source /home/toby/projects/eve-codes/scripts/retry.sh
source /home/toby/projects/eve-codes/scripts/secrets.sh

# Jita
# Heimatar
# Domain
# Sinq Laison
# Metropolis
# Others
for REGION in 10000002 10000030 10000043 10000032 10000042 -1
do
   retry 5 /home/toby/.local/bin/eve-orders --host="log" --dbname="eve" --region=$REGION --types="buy" &&
   retry 5 /home/toby/.local/bin/eve-orders --host="log" --dbname="eve" --region=$REGION --types="sell"
done
