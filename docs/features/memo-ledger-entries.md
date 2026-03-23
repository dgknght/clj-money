# Memo Ledger Entry

A stock split does not change the value of the an account and therefore has no
associated transaction. However, we need a way to record that fact that the
stock split occurred, beyond the in-place changes in the lot.

When a stock splits:
- Record a memo ledger entry with the date of the split and a note about the
  split. This is a new entity in the system.
