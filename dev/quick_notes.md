# Dev Quick Notes

## TODO

* More tests for `column_orderings()` helper functions.
* Pass `...` in `equal_elements()` to all.equal (issue with map).
* Clean dev_code.R. There's stuff on functions which I've already written.

## Notes

I created `equal_elements()` as a helper for `column_orderings()` to deal with columns which were the 
same. My intention was to remove all but one of each set of equal columns before producing the paths. However, doing this would have been flawed since the equality measure with NAs ignored would not be 
transitive. Instead, I kept all paths and had `equal_elements()` treat data frames as lists.




