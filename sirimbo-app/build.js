const path = require("path");
const esbuild = require("esbuild");
const autoprefixer = require("autoprefixer");
const postCssPlugin = require("@deanc/esbuild-plugin-postcss");
const sassPlugin = require("esbuild-sass-plugin").sassPlugin;
const alias = require('esbuild-plugin-alias');
const svgrPlugin = require('esbuild-plugin-svgr');

esbuild.build({
    entryPoints: ["src/main.tsx", "src/index.tsx"],
    sourcemap: true,
    bundle: true,
    publicPath: '/',
    outdir: "dist/",
    loader: {
        '.eot': 'file',
        '.woff': 'file',
        '.woff2': 'file',
        '.ttf': 'file',
        '.jpg': 'file',
        '.png': 'file',
    },
    define: {
        global: 'window',
    },
  plugins: [
    svgrPlugin({
      svgoConfig: {
        plugins: ['preset-default'],
      },
      svgo: true,
    }),
    sassPlugin(),
    postCssPlugin({
      plugins: [autoprefixer],
    }),
    alias({
      'images/layers.png': path.resolve(__dirname, '../../node_modules/leaflet/dist/images/layers.png'),
      'images/layers-2x.png': path.resolve(__dirname, '../../node_modules/leaflet/dist/images/layers-2x.png'),
      'images/marker-icon.png': path.resolve(__dirname, '../../node_modules/leaflet/dist/images/marker-icon.png'),
      'images/marker-icon-2x.png': path.resolve(__dirname, '../../node_modules/leaflet/dist/images/marker-icon-2x.png'),
      'images/marker-shadow.png': path.resolve(__dirname, '../../node_modules/leaflet/dist/images/marker-shadow.png')
    }),
  ],
}).catch(
    (e) => console.error(e.message)
);
