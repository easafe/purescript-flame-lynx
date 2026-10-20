import { createRequire } from 'node:module';
import { defineConfig } from 'vitest/config';
import { vitestTestingLibraryPlugin } from '@lynx-js/react/testing-library/plugins';

let require = createRequire(import.meta.url);

function filterLynxSourcemapWarnings(config) {
    let warnOnce = config.logger.warnOnce;

    config.logger.warnOnce = function (message, options) {
        if (
            message.startsWith('Sourcemap for "') &&
            message.includes('/node_modules/@lynx-js/react/') &&
            message.endsWith('points to missing source files')
        )
            return;

        warnOnce(message, options);
    };
}

export default defineConfig({
    // ESM lets Lynx's official React alias apply inside React Router too
    resolve: {
        alias: {
            'react-router': require.resolve('react-router').replace('/dist/main.js', '/dist/index.js'),
        },
    },
    plugins: [
        vitestTestingLibraryPlugin(),
        {
            name: 'filter-lynx-sourcemap-warnings',
            configResolved: filterLynxSourcemapWarnings,
        },
    ],
    test: {
        include: ['test/index.js'],
        testTimeout: 15000,
        server: { deps: { inline: ['react-router'] } },
    },
});
