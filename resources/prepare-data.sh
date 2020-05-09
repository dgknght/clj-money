echo 'Clearing the workspace...'
rm target/sample-data*
echo 'Copying the file from source...'
cp "$1" target/sample-data.xml.gz
echo 'Unzipping the file...'
gunzip target/sample-data.xml.gz
echo 'Removing prices...'
sed "2838,$(grep -n \/gnc:pricedb target/sample-data.xml | cut -d ':' -f 1)d" target/sample-data.xml > target/sample-data-without-prices.xml
echo 'Zipping the file...'
gzip target/sample-data-without-prices.xml
mv target/sample-data-without-prices.xml.gz target/sample-data-without-prices.gnucash
echo 'Chunking the file...'
lein chunk-file target/sample-data-without-prices.gnucash
