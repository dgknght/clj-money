select a.name,
  i.index,
  t.transaction_date,
  t.description,
  i.action,
  i.quantity,
  i.balance item_balance,
  a.quantity account_quantity,
  a.value account_value
from transaction_items i
  inner join transactions t on t.id = i.transaction_id
    and t.transaction_date = i.transaction_date
  inner join accounts a on a.id = i.account_id
order by a.id, i.index;
