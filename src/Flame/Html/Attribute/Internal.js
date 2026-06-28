export function createProp(name) {
    return function (value) {
        return [name, value];
    };
}

export function createClass(array) {
    return ['className', array.join('')];
}

export function createStyle(object) {
    return ['style', object];
}
