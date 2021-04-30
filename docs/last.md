
## Last Action
- [x] generate openapi schema
- [x] show project by key (shows nothing)
- [x] add nice formatting
- [x] add showing docs by hover (work for my code and GHC.Base)
- [x] make serialization for project as simple as possible
- [ ] Make MongoDB doc id as Entity ID
- [ ] make identifier (key) from string: replace spaces with underscores and etc.
- [x] decide how to pass schema to storage
Schema ends in functions. After this we pass correct issue to store. Key should be craeted
- [ ] transform DB Action to Either monad for error passing


## Boilerplate

- [ ] auto generation fromSchema/toSchema from fromJSON/toJSON. 
- [ ] override fromBSON only one field (mongo id as entity key, other - by default)
