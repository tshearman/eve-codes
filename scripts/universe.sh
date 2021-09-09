#!/usr/bin/env bash

source /home/toby/projects/eve-codes/scripts/retry.sh
source /home/toby/projects/eve-codes/scripts/secrets.sh

retry 5 /home/toby/.local/bin/eve-universe --host="log" --dbname="eve" --endpoint="kills" &&
retry 5 /home/toby/.local/bin/eve-universe --host="log" --dbname="eve" --endpoint="jumps"
