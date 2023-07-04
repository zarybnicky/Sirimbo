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

  fs.mkdirSync("../res");
  fs.copyFileSync(require.resolve('graphile-build-pg/res/watch-fixtures.sql'), 'dist/watch-fixtures.sql');

  fs.mkdirSync("./sql");
  for (let i = 1; ; i++) {
    try {
      let name = i.toString();
      name = "000000".substring(name.length) + name;
      fs.copyFileSync(require.resolve(`graphile-worker/sql/${name}.sql`), `sql/${name}.sql`);
    } catch (e) {
      console.log(e);
      if (i == 1) throw e;
      break;
    }
  }

  fs.mkdirSync("./dist/templates");
  for (const file of fs.readdirSync("src/tasks/templates")) {
    fs.copyFileSync(`src/tasks/templates/${file}`, `dist/templates/${file}`);
  }
})()
