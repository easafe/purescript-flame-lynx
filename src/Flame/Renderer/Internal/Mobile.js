import { root, createElement, useEffect, useState } from '@lynx-js/react';
import { MemoryRouter, Routes, Route, useNavigate, useLocation } from 'react-router';

let navigate;

function RouterContent({ applications }) {
    let routerNavigate = useNavigate();

    useEffect(() => {
        navigate = routerNavigate;

        return () => { navigate = undefined; };
    }, [routerNavigate]);

    return createElement(Routes, null,
        applications.map(application => createElement(Route, {
            key: application.path,
            path: application.path,
            element: createElement(GuardedApplication, { application }),
        })));
}

function GuardedApplication({ application }) {
    let location = useLocation(),
        routerNavigate = useNavigate(),
        [allowedKey, setAllowedKey] = useState(null);

    useEffect(() => {
        let destination = application.guard();

        if (destination !== null && routePath(destination) !== location.pathname) {
            setAllowedKey(null);
            // wait for subscription before setting navigation
            let redirect = setTimeout(() => routerNavigate(routePath(destination), { replace: true }), 0);

            return () => clearTimeout(redirect);
        } else {
            application.initialize();
            setAllowedKey(location.key);
        }

    }, [application, location.key, location.pathname, routerNavigate]);

    return allowedKey === location.key ? createElement(application.App) : null;
}

function routePath(path) {
    return path.startsWith('/') ? path : '/' + path;
}

export function start_(applications) {
    root.render(createElement(MemoryRouter, { initialEntries: [applications[0].path] }, createElement(RouterContent, { applications })));
}

export function create_(appId, updater, view, initialState, guard) {
    return new M(appId, updater, view, initialState, guard);
}

export function resume_(m, state) {
    m.resume(state);
}

export function navigate_(path) {
    if (!navigate)
        throw new Error('Flame router is not mounted');

    navigate(routePath(path));
}

export function getState_(m) {
    return m.getState();
}

export function enqueue_(m, effect) {
    m.enqueue(effect);
}

class M {
    constructor(appId, updater, view, initialState, guard) {
        this.path = routePath(appId);
        this.guard = guard;
        this.initialized = false;
        this.pending = [];
        this.flushing = false;

        this.initialize = () => {
            if (this.initialized)
                return;

            this.state = initialState;
            this.initialized = true;
            this.flush();
        };
        //a route isn't initialized until its guard passes
        // so subscriptions need to be queued in case they were sent before that
        this.enqueue = (effect) => {
            this.pending.push(effect);
            this.flush();
        };
        this.flush = () => {
            if (!this.initialized || this.flushing)
                return;

            this.flushing = true;
            let processed = 0;

            try {
                while (processed < this.pending.length)
                    this.pending[processed++]();
            } finally {
                this.pending.splice(0, processed);
                this.flushing = false;
            }
        };
        this.getState = () => this.state;
        this.resume = (state) => {
            this.state = state;

            if (this.setState !== undefined)
                this.setState(() => state);
        };
        this.App = () => {
            let [state, setState] = useState(() => this.state);

            this.setState = setState;

            useEffect(() => () => {
                this.setState = undefined;
            }, []);

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

        let update = updater(this);

        function toProps(nodeData) {
            let props = {};

            for (let [name, value, isEvent] of nodeData) {
                if (isEvent) {
                    props[name] = typeof value === "function" ? (e) => update(value(e)())() : () => update(value)();
                }
                else
                    props[name] = value;
            }

            return props;
        }
    }
}
