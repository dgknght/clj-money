echo "Migrating PMP LCC..."
lein migrate-account -- -u dougbknight@gmail.com -e "$1" -f "UBS Bank USA Deposit" -t "PMP LCC"
echo "Migrating PMP ACG..."
lein migrate-account -- -u dougbknight@gmail.com -e "$1" -f "USA Deposit Account" -t "PMP ACG"
echo "Removing unused commodity accounts..."
psql -U app_user -h localhost -c "delete from account where commodity_id in (select id from commodity where name = 'Vanguard Moderate Age-Based Option') and id not in (select account_id from transaction_item);" -d money_development
echo "Done."
