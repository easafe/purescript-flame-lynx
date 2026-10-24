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
        } else
            return application.initialize(() => setAllowedKey(location.key));
    }, [application, location.key, location.pathname, routerNavigate]);

    return allowedKey === location.key ? createElement(application.App) : null;
}

function routePath(path) {
    return path.startsWith('/') ? path : '/' + path;
}

export function start_(applications) {
    root.render(createElement(MemoryRouter, { initialEntries: [applications[0].path] }, createElement(RouterContent, { applications })));
}

export function create_(appId, initialize, guard) {
    return new M(appId, (m, ready, failed) => initialize(m)(failed)(application => () => ready(application))(), guard);
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

class M {
    constructor(appId, load, guard) {
        this.path = routePath(appId);
        this.guard = guard;
        this.initialization = 'idle';
        this.ready = undefined;

        this.initialize = (ready) => {
            if (this.initialization === 'ready') {
                ready();

                return () => {};
            }

            this.ready = ready;

            if (this.initialization === 'idle') {
                this.initialization = 'loading';

                load(this, application => {
                    this.state = application.model;
                    this.view = application.view;
                    this.update = application.update;
                    this.initialization = 'ready';
                    this.ready?.();
                    this.ready = undefined;
                }, () => {
                    this.initialization = 'idle';
                    this.ready = undefined;
                });
            }

            return () => {
                if (this.ready === ready)
                    this.ready = undefined;
            };
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

                if (html[0] === 'text' && typeof html[2] === 'string')
                    children = html[2];
                else if (html[2] !== undefined) {
                    children = new Array(html[2].length);

                    for (let i = 0; i < html[2].length; ++i)
                        children[i] = toReact(html[2][i]);
                }

                return createElement(html[0], props, children);
            }

            return toReact(this.view(state));
        };

        let update = message => this.update(message);

        function toProps(nodeData) {
            let props = Object.create(null),
                events = Object.create(null);

            for (let [name, value, isEvent] of nodeData) {
                if (isEvent) {
                    if (events[name] === undefined)
                        events[name] = [];

                    events[name].push(value);
                }
                else
                    props[name] = value;
            }

            for (let name of Object.keys(events)) {
                let handlers = events[name];

                props[name] = event => {
                    for (let handler of handlers)
                        update(typeof handler === "function" ? handler(event)() : handler)();
                };
            }

            return props;
        }
    }
}
