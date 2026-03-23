#!/bin/bash

echo "Migrating PMP LCC..."
lein migrate-account -- -u dougbknight@gmail.com -e "$1" -f "UBS Bank USA Deposit" -t "PMP LCC"
echo "Migrating PMP ACG..."
lein migrate-account -- -u dougbknight@gmail.com -e "$1" -f "USA Deposit Account" -t "PMP ACG"
echo "Done."
