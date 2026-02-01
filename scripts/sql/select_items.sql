select a.name,
  i.index,
  t.transaction_date,
  i.reconciliation_id,
  t.description,
  i.action,
  i.quantity,
  i.balance item_balance,
  a.quantity account_quantity,
  a.value account_value
from account_item i
  inner join transaction_item ti on ti.id = i.transaction_item_id
  inner join transaction t on t.id = ti.transaction_id
  inner join account a on a.id = i.account_id
order by a.id, i.index;
