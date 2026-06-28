import { root, createElement, useState } from '@lynx-js/react'

export function start_(updater, view, initialState) {
    return new M(updater, view, initialState);
}

export function resume_(m, state) {
    m.resume(state);
}

export function getState_(m) {
    return m.getState();
}

class M {
    constructor(updater, view, initialState) {
        this.App = () => {
            let [state, setState] = useState(initialState);

            this.getState = () => state;
            this.resume = (s) => setState(s);

            function toReact(html) {
                let props = html[1] === undefined ? undefined : toProps(html[1]),
                    children;

                if (html[0] === 'text')
                    children = html[2];
                else if (html[2] !== undefined) {
                    children = new Array(html[2].length);

                    for (let i = 0; i < html[2].length; ++i)
                        children[i] = toReact(html[2][i]);
                }

                return createElement(html[0], props, children);
            }

            return toReact(view(state));
        };

        function toProps(nodeData) {
            let props = {};

            for (let [name, value, isEvent] of nodeData) {
                if (isEvent) {
                    props[name] = typeof value === "function" ? (e) => updater(value(e))() : () => updater(value)();
                }
                else
                    props[name] = value;
            }

            return props;
        }

        root.render(createElement(this.App));
    }
}
