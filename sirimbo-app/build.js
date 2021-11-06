const esbuild = require("esbuild");
const autoprefixer = require("autoprefixer");
const postCssPlugin = require("@deanc/esbuild-plugin-postcss");
const sassPlugin = require("esbuild-sass-plugin").sassPlugin;

esbuild.build({
    entryPoints: ["src/index.tsx"],
    sourcemap: true,
    bundle: true,
    outfile: "dist/main.js",
    loader: {
        '.eot': 'file',
        '.woff': 'file',
        '.woff2': 'file',
        '.ttf': 'file',
        '.png': 'file',
        '.svg': 'file',
    },
    define: {
        global: 'window',
    },
    plugins: [
        sassPlugin(),
        postCssPlugin({
            plugins: [autoprefixer],
        }),
    ],
}).catch(
    (e) => console.error(e.message)
);
