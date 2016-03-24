# Quash

A JavaScript library to help me keep my job.

# Usage

This is a thunk-compliant error quasher. Give it a [thunk][] and stop worrying.

```javascript
var quash = require('quash');

quash(function () {
    // Read from database.
    // Process data.
    I should have been a pair of ragged claws, scuttling across the floors of silent seas...
    // Save records.
});
```

[thunk]: https://en.wikipedia.org/wiki/Thunk
