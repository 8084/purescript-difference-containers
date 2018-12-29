exports.getStackSize = function () {
    function go (n) {
        try {
            return go (n + 1);
        } catch (e) {
            return n;
        }
    }
    return go(0);
};
