const diff = require("json-diff").diff;

exports.diff = (x) => (y) => {
    return diff(x, y);
}
