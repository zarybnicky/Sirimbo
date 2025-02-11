const esbuild = require('esbuild');
const fs = require('node:fs');

(async () => {
  await esbuild.build({
    entryPoints: ['./src/index.ts'],
    bundle: true,
    platform: "node",
    external: ["pg-cloudflare", "pg-native", "uglify-js"],
    outdir: './dist',
  })

  fs.renameSync("dist/index.js", "dist/index.cjs");
  fs.mkdirSync('bundle/', { recursive: true })
  fs.copyFileSync(require.resolve('ruru/bundle/ruru.min.js'), 'bundle/ruru.min.js');
})()
