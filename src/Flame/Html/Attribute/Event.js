export function createEvent_(f) {
    return function (name) {
        return function (message) {
            return [name, f(message), true];
        }
    };
}

export function createRawEvent_(name) {
    return function (handler) {
        return [name, handler, true];
    };
}

export function nodeValue_(event) {
    return event.detail.value;
}
