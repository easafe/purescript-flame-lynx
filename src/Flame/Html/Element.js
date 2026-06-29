export function text(value) {
    return ['text', undefined, value];
}

export function createElementNode(tag) {
    return function (nodeData) {
        return function (children) {
            return [tag, nodeData, children];
        };
    };
}

export function createDatalessElementNode(tag) {
    return function (children) {
        return [tag, undefined, children];
    };
}

export function createSingleElementNode(tag) {
    return function (nodeData) {
        return [tag, nodeData, undefined];
    };
}

// export function createEmptyElement(tag) {
//     return {
//         nodeType: tag.trim().toLowerCase() === 'svg' ? svgNode : elementNode,
//         node: undefined,
//         tag: tag,
//         nodeData: {}
//     };
// }

// export function createLazyNode(nodeData) {
//     return function (render) {
//         return function (arg) {
//             let key = nodeData[0];

//             return {
//                 nodeType: lazyNode,
//                 node: undefined,
//                 nodeData: key === undefined ? undefined : { key: key },
//                 render: render,
//                 arg: arg,
//                 rendered: undefined
//             };
//         };
//     };
// }

// export function createManagedNode(render) {
//     return function (nodeData) {
//         return function (arg) {
//             return {
//                 nodeType: managedNode,
//                 node: undefined,
//                 nodeData: fromNodeData(nodeData),
//                 createNode: render.createNode,
//                 updateNode: render.updateNode,
//                 arg: arg
//             };
//         };
//     };
// }

// export function createDatalessManagedNode(render) {
//     return function (arg) {
//         return {
//             nodeType: managedNode,
//             node: undefined,
//             nodeData: {},
//             createNode: render.createNode,
//             updateNode: render.updateNode,
//             arg: arg
//         };
//     };
// }

export function createSvgNode(nodeData) {
    return function (children) {
        let props = "",
            nd = [];

        for (let [name, value, isEvent] of nodeData)
            if (!isEvent && name !== "className" && name !== "style")
                props += `${name}="${value}" `;
            else
                nd.push([name, value, isEvent]);

        //thats how lynx parses svg, with a content prop for the markup
        nd.push(["content", `<svg ${props}>${children}</svg>`]);

        return ["svg", nd, undefined];
    };
}

export function createSvgChild(tag) {
    return function (nodeData) {
        return function (children) {
            let props = '';

            for (let [name, value, isEvent] of nodeData)
                if (!isEvent)
                    props += `${name}="${value} "`;

            let end = children.length == 0 ? '/>' : `${children.join('')}</${tag}>`;

            return `<${tag} ${props} ${end}`;
        }
    }
}

// export function createDatalessSvgNode(children) {
//     return {
//         nodeType: svgNode,
//         node: undefined,
//         tag: 'svg',
//         nodeData: {},
//         children: asSvg(children)
//     };
// }

// export function createSingleSvgNode(nodeData) {
//     return {
//         nodeType: svgNode,
//         node: undefined,
//         tag: 'svg',
//         nodeData: fromNodeData(nodeData)
//     };
// }

