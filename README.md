# hask-task-tracker

# How diener works

- declare api with servant
- declare typeclasses in MongoDB.Class for simplify `insert` `getAll`
  (de)serialize from/to mongo

`ReadOnly` - Monad that instantiate this class, can call `getAll` and gets content from mongo

