export function createCustomListener_(eventName, updater) {
    'background only';

    lynx.getJSModule('GlobalEventEmitter').addListener(eventName, function (payload) {
        updater(payload)();
    });
}

export function dispatchCustomEvent_(eventName, payload) {
    'background only';

    lynx.getJSModule('GlobalEventEmitter').trigger(eventName, payload);
}
