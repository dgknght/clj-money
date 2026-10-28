# Statement fixtures

Fake bank, credit card, and brokerage statements for testing statement
reconciliation (#182, #211). The banks, people, account numbers, and
transactions are all fictitious. Their layouts were modeled on real statements, which must never
be committed.

- `*.html` - the source for each statement
- `*.pdf` - the rendered statement, as a user would upload it
- `*.json` - the response we expect from the model for that statement.
  Amounts are signed by their effect on the account balance, so a deposit to
  checking and a charge on a credit card are both positive.

To regenerate a PDF after editing its HTML:

```bash
libreoffice --headless --convert-to pdf:writer_web_pdf_Export checking.html
```

Things the statements exercise on purpose:

- dates printed without a year (`08/21`)
- unsigned amounts whose direction comes from the section they appear in
- a card purchase posted a few days after it was made (Corner Hardware,
  09/05 purchased, 09/08 posted)
- two identical amounts in one period (Zelle payments of $75.00)
- a transfer that appears on both statements (the $612.35 card payment)
- a credit on a credit card (the Amazon return)
- a brokerage purchase dated a day after it was recorded (the AAPL
  purchase, recorded on 09/03, shown on its trade date, 09/04)
- a dividend and its reinvestment, listed as two lines, read as one
- a sale and a fee that aren't recorded yet
- the cash balance, which is less than the account's total value
