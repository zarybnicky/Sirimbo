const { globSync } = require('glob');
const esbuild = require('esbuild');

(async () => {
  await esbuild.build({
    entryPoints: ["graphile.config.ts"],
    allowOverwrite: true,
    bundle: true,
    platform: "node",
    external: ["pg-cloudflare", "pg-native", "uglify-js"],
    outdir: '.',
  })
  await esbuild.build({
    entryPoints: globSync('tasks/**/*.ts'),
    allowOverwrite: true,
    bundle: true,
    platform: "node",
    external: ["pg-cloudflare", "pg-native", "uglify-js"],
    outdir: './tasks',
  })
})()
