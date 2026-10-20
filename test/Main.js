import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

export function main() {
    let cli = fileURLToPath(import.meta.resolve('vitest/vitest.mjs')),
        result = spawnSync(process.execPath, [cli, 'run', ...process.argv.slice(2)], { stdio: 'inherit' });

    if (result.error)
        throw result.error;

    process.exitCode = result.status ?? 1;
}
