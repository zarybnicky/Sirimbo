import * as esbuild from 'esbuild'
import fs from 'node:fs'

await esbuild.build({
  entryPoints: ['./src/index.ts'],
  bundle: true,
  platform: "node",
  external: ["pg-cloudflare", "pg-native", "uglify-js"],
  outdir: 'dist',
})

// copy SQL
