export function createProperty(name) {
    return function (value) {

        return [name, value];
    };
}

export function createClass(array) {
    return ['className', array.join(' ')];
}

export function createStyle(object) {
    return ['style', object];
}

export { createProperty as createBooleanProp, createProperty as createIntProp };
