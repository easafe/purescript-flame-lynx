import * as LR from '@lynx-js/react';

let navigate;

function Router({ applications }) {
    let [active, setActive] = LR.useState(null),
        reference = LR.useRef({ active: null, pending: null }),
        controller = reference.current;

    controller.setActive = setActive;
    LR.useEffect(() => {
        let go = path => showApplication(controller, applications, path);

        navigate = go;
        go(applications[0].path);

        return () => {
            cancelPending(controller);
            if (navigate === go)
                navigate = undefined;
        };
    }, [applications]);

    return active === null ? null : LR.createElement(active.App);
}

function showApplication(controller, applications, path) {
    let application = resolveApplication(applications, path);

    if (controller.pending?.application === application)
        return;

    cancelPending(controller);

    if (controller.active === application)
        return;

    let request = { application };

    controller.pending = request;

    if (controller.active && application.initialization !== 'ready') {
        let destination = controller.active.guard();

        if (destination !== null && destination !== controller.active.path) {
            controller.active = null;
            controller.setActive(null);
        }
    }

    request.cancel = application.initialize(() => {
        if (controller.pending !== request)
            return;

        let allowed = resolveApplication(applications, application.path);

        controller.pending = null;
        if (allowed !== application) {
            showApplication(controller, applications, allowed.path);
            return;
        }

        controller.active = application;
        controller.setActive(application);
    }, () => {
        if (controller.pending === request)
            controller.pending = null;
    });
}

function resolveApplication(applications, path) {
    let visited = new Set();

    while (!visited.has(path)) {
        let application = applications.find(a => a.path === path);
        if (!application)
            throw new Error('Unknown Flame route: ' + path);

        let destination = application.guard();
        if (destination === null || destination === path)
            return application;

        visited.add(path);
        path = destination;
    }
    throw new Error('Circular Flame route guard: ' + path);
}

function cancelPending(controller) {
    controller.pending?.cancel?.();
    controller.pending = null;
}

export function start_(applications) {
    LR.root.render(LR.createElement(Router, { applications }));
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

    navigate(path);
}

export function getState_(m) {
    return m.state;
}

class M {
    constructor(appId, load, guard) {
        this.path = appId;
        this.guard = guard;
        this.initialization = 'idle';
        this.listener = undefined;
        this.state = undefined;
        this.setState = undefined;

        this.initialize = (ready, failed) => {
            if (this.initialization === 'ready') {
                ready();
                return () => {};
            }

            let listener = { ready, failed };

            this.listener = listener;

            if (this.initialization === 'idle') {
                this.initialization = 'loading';

                load(this, application => {
                    this.state = application.model;
                    this.view = application.view;
                    this.update = application.update;
                    this.initialization = 'ready';
                    this.listener?.ready();
                    this.listener = undefined;
                }, () => {
                    this.initialization = 'idle';
                    this.listener?.failed();
                    this.listener = undefined;
                });
            }

            return () => {
                if (this.listener === listener)
                    this.listener = undefined;
            };
        };
        this.resume = (state) => {
            this.state = state;

            if (this.setState !== undefined)
                this.setState(() => state);
        };
        this.App = () => {
            let [state, setState] = LR.useState(() => this.state);

            this.setState = setState;

            LR.useEffect(() => () => {
                if (this.setState === setState)
                    this.setState = undefined;
            }, []);

            return toReact(this.view(state));
        };

        let update = message => this.update(message);

        function toReact(html) {
            let props = html[1] === undefined ? undefined : toProps(html[1]),
                children = html[0] === 'text' && typeof html[2] === 'string'
                    ? html[2]
                    : html[2]?.map(toReact);

            return LR.createElement(html[0], props, children);
        }

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
