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
        return {
            node: undefined,
            tag: 'svg',
            nodeData: nodeData,
            children: children
        };
    };
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

