curl --request GET \
    --url 'https://alpha-vantage.p.rapidapi.com/query?function=DIGITAL_CURRENCY_DAILY&symbol=BTC&market=USD' \
      --header 'x-rapidapi-host: alpha-vantage.p.rapidapi.com' \
        --header 'x-rapidapi-key: $YAHOO_API_KEY'
