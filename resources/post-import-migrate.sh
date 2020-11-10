echo "Migrating PMP LCC..."
lein migrate-account -- -u dougbknight@gmail.com -e "ZZZ Real World" -f "UBS Bank USA Deposit" -t "PMP LCC"
echo "Migrating PMP ACG..."
lein migrate-account -- -u dougbknight@gmail.com -e "ZZZ Real World" -f "USA Deposit Account" -t "PMP ACG"
echo "Removing unused commodity accounts..."
psql -c "delete from accounts where commodity_id in (select id from commodities where name = 'Vanguard Moderate Age-Based Option') and id not in (select account_id from transaction_items);" -d money_development
echo "Done."
