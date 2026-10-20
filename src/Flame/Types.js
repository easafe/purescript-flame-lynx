export function messageMapper(mapper) {
    return function mapHtml(html) {
        // SVG children are serialized markup rather than event-bearing nodes
        if (typeof html === 'string')
            return html;

        let [tag, nodeData, children] = html;

        let mappedData = nodeData?.map(([name, value, isEvent]) => {
            if (!isEvent)
                return [name, value];

            let mapped = typeof value === 'function'
                ? event => () => mapper(value(event)())
                : mapper(value);

            return [name, mapped, true];
        });

        return [tag, mappedData, Array.isArray(children) ? children.map(mapHtml) : children];
    };
}
