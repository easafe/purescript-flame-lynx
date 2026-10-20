import { test, describe, afterAll } from 'vitest';
import { createElement, Fragment } from '@lynx-js/react';
import { act, cleanup, render, fireEvent, createEvent } from '@lynx-js/react/testing-library';

function runAsync(run) {
    return function () {
        return new Promise((resolve, reject) => {
            run(() => resolve())(error => {
                return function () {
                    reject(error);
                };
            })();
        });
    };
}

export function registerTest_(name) {
    return function (run) {
        return function () {
            return test(name, runAsync(run));
        };
    };
}

export function describe_(name) {
    return function (body) {
        return function () {
            return describe(name, body);
        };
    };
}

export function pending_(name) {
    return function () {
        return test.skip(name, () => {});
    };
}

export function afterAll_(run) {
    return function () {
        return afterAll(runAsync(run));
    };
}

export function unsafeCreateEnviroment() {
    cleanup();
    lynxTestingEnv.reset();
    render(createElement(Fragment, null));
}

export function perform_(effect) {
    return function (onError, onSuccess) {
        let cancelled = false, result;

        Promise.resolve().then(() => {
            return act(() => {
                result = effect();
            });
        }).then(
            () => {
                if (!cancelled)
                    onSuccess(result);
            },
            error => {
                if (!cancelled)
                    onError(error);
            },
        );

        return function (_error, _onError, onSuccess) {
            cancelled = true;
            onSuccess();
        };
    };
}

export function getStyle_(element) {
    return function (name) {
        return function () {
            return element.style.getPropertyValue(name);
        };
    };
}

export function dispatch_(selector) {
    return function (name) {
        return function (value) {
            return function () {
                let element = document.querySelector(selector),
                    event = createEvent(`bindEvent:${name}`, element, { bubbles: true });

                Object.assign(event, { eventType: 'bindEvent', eventName: name, detail: { value } });
                fireEvent(lynx.createSelectorQuery().select(selector), event);
            };
        };
    };
}

// Preserve native property types, which DOM serialization cannot observe
export function attributeValue_(attribute) {
    return JSON.stringify(attribute[1]);
}

export function attributeName_(attribute) {
    return attribute[0];
}

export function settle_(onError, onSuccess) {
    let cancelled = false;

    Promise.resolve(act(async () => {
        await new Promise(resolve => setTimeout(resolve, 20));
    })).then(() => {
        if (!cancelled)
            onSuccess();
    }, onError);

    return function (_error, _onError, success) {
        cancelled = true;
        success();
    };
}
