const path = require("path");
const fs = require("fs");
const webpack  = require('webpack');
const CopyPlugin = require("copy-webpack-plugin");
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");
const { BundleAnalyzerPlugin } = require('webpack-bundle-analyzer');

module.exports = {
  mode: 'development',
  entry: {
    index: './src/index',
    main: './src/main',
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    publicPath: '/',
    assetModuleFilename: 'assets/[name][ext][query]',
  },
  resolve: {
    symlinks: false,
    extensions: ['.js', '.ts', '.tsx', '.json'],
  },
  devtool: "source-map",
  devServer: {
    hot: 'only',
    historyApiFallback: true,
    static: {
      directory: path.join(__dirname, 'static'),
    },
    proxy: {
      '/graphql': 'http://localhost:3000',
      '/graphiql': 'http://localhost:3000',
    },
  },
  plugins: [
    new MiniCssExtractPlugin(),
    new CopyPlugin({ patterns: [{ from: "static" }] }),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery',
      'window.jQuery': 'jquery',
      Popper: ['popper.js', 'default'],
    }),
    // new BundleAnalyzerPlugin({ analyzerMode: 'static', openAnalyzer: false }),
  ],
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        exclude: path.resolve(__dirname, 'node_modules'),
        use: 'ts-loader',
      },
      {
        test: /\.(png|jpe?g|gif|ttf)$/i,
        use: 'file-loader',
      },
      {
        test: /\.svg$/i,
        use: ["@svgr/webpack"],
      },
      {
        test: /\.scss$/,
        include: path.resolve(__dirname, 'src'),
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
