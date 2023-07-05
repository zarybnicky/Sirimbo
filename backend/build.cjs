const esbuild = require('esbuild');
const fs = require('node:fs');
const childProcess = require('node:child_process');

(async () => {
  await esbuild.build({
    entryPoints: ['./src/index.ts', "./src/worker-schema"],
    bundle: true,
    platform: "node",
    external: ["pg-cloudflare", "pg-native", "uglify-js"],
    outdir: './dist',
  })

  fs.renameSync("dist/index.js", "dist/index.cjs");
  fs.renameSync("dist/worker-schema.js", "dist/worker-schema.cjs");
  childProcess.execSync("sed -i s@/../sql@/sql@g dist/index.cjs")
  childProcess.execSync("sed -i s@../../res/@./@ dist/index.cjs")

  fs.copyFileSync(require.resolve('graphile-build-pg/res/watch-fixtures.sql'), 'dist/watch-fixtures.sql');

  fs.mkdirSync("./dist/sql", { recursive: true });
  for (let i = 1; ; i++) {
    try {
      let name = i.toString();
      name = "000000".substring(name.length) + name;
      fs.copyFileSync(require.resolve(`graphile-worker/sql/${name}.sql`), `dist/sql/${name}.sql`);
    } catch (e) {
      if (i == 1) throw e;
      break;
    }
  }

  fs.mkdirSync("./dist/templates", { recursive: true });
  for (const file of fs.readdirSync("src/tasks/templates")) {
    fs.copyFileSync(`src/tasks/templates/${file}`, `dist/templates/${file}`);
  }
})()
