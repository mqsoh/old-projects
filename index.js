module.exports = function (thunk) {
    try {
        thunk();
    } catch (err) {
        // "Doesn't" matter.
    }
};
