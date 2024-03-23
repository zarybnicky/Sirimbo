const esbuild = require('esbuild');
const fs = require('node:fs');
const childProcess = require('node:child_process');

(async () => {
  await esbuild.build({
    entryPoints: ['./src/index.ts'],
    bundle: true,
    platform: "node",
    external: ["pg-cloudflare", "pg-native", "uglify-js"],
    outdir: './dist',
  })

  fs.renameSync("dist/index.js", "dist/index.cjs");
  childProcess.execSync("sed -i s@/../sql@/sql@g dist/index.cjs")
  childProcess.execSync("sed -i s@../../res/@./@ dist/index.cjs")

  fs.copyFileSync(require.resolve('graphile-build-pg/res/watch-fixtures.sql'), 'dist/watch-fixtures.sql');
})()
