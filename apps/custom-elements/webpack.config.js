const path = require("path");
const process = require("process");
const webpack  = require('webpack');
const CopyPlugin = require("copy-webpack-plugin");
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");

module.exports = {
  mode: 'production',
  entry: {
    main: './main',
  },
  output: {
    path: path.resolve(process.cwd(), 'dist'),
    publicPath: '/',
    assetModuleFilename: 'assets/[name][ext][query]',
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx', '.json'],
  },
  devtool: "source-map",
  plugins: [
    new MiniCssExtractPlugin(),
    new CopyPlugin({ patterns: [{ from: "static" }] }),
    new webpack.DefinePlugin({
      'process.env.PORT': 'undefined',
      'process.env.GRAPHQL_BACKEND': 'undefined',
      'process.env.NEXT_PUBLIC_GRAPHQL_BACKEND': 'undefined',
      'process.env.NEXT_PUBLIC_TENANT_ID': '1',
    }),
    new webpack.NormalModuleReplacementPlugin(/next\/link/, '@app/ui/next-link-dummy.ts'),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
      'window.jQuery': 'jquery',
      Popper: ['popper.js', 'default'],
    }),
    // new (require('webpack-bundle-analyzer').BundleAnalyzerPlugin)()
  ],
  module: {
    rules: [
      {
        test: /\.[jt]sx?$/,
        loader: 'esbuild-loader',
        options: {
          target: 'es2015'
        }
      },
      {
        test: /\.(png|jpe?g|gif|eot|woff2|woff|ttf|svg)$/i,
        type: 'asset/resource',
      },
      {
        test: /\.scss$/,
        use: [MiniCssExtractPlugin.loader, 'css-loader', 'postcss-loader', 'sass-loader'],
        sideEffects: true,
      },
      {
        test: /\.css$/,
        use: [MiniCssExtractPlugin.loader, 'css-loader', 'postcss-loader'],
        sideEffects: true,
      },
    ],
  },
  optimization: {
    minimize: true,
    minimizer: [
      new CssMinimizerPlugin({
        minimizerOptions: {
          preset: [
            "default",
            {
              discardComments: { removeAll: true },
            },
          ],
        },
      }),
    ],
  },
};
