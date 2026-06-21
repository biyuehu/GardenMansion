# Todo

## Backend

- [X] Implement hight-level logger
- [X] `User` model add `nickname` field which is changeable by user
- [X] Remove `alive` and `admin` fileds of `User` model, add `level` field (base on aglebra data type, it seems to need update `type-generator` script)
- [X] handler insreting new data need find max id and add 1 to it, then insert new data with new id
- [X] Forbid not active user to login or use
- [X] `when` replace all `if-else-then` in handlers logic
