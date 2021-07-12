/*

Sqlite virtual table for editing configuration files.
Basic principles:

- A config file is a collection of _flat_ key-value pairs.
  ( Note that this is too hard to make hierarchical, so if
  hierarchy is needed, create multiple tables. )

- Transactions use the vtab transaction mechanism: "commit" writes out
  the new config file.

*/



