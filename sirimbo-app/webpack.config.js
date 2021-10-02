const path = require("path");
const fs = require("fs");
const webpack  = require('webpack');
const CopyPlugin = require("copy-webpack-plugin");
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
  mode: 'development',
  entry: './src/index',
  output: {
      filename: 'main.js',
      path: path.resolve(__dirname, 'dist'),
      publicPath: '/',
      assetModuleFilename: 'assets/[name][ext][query]',
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx', '.json']
  },
  devtool: "source-map",
  devServer: {
    contentBase: './',
  },
  plugins: [
      new MiniCssExtractPlugin({ filename: 'main.css' }),
      new CopyPlugin({ patterns: [{ from: "static" }] }),
      new webpack.ProvidePlugin({
          $: 'jquery',
          jQuery: 'jquery',
          'window.jQuery': 'jquery',
          Popper: ['popper.js', 'default'],
      }),
  ],
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
      },
      {
          test: /\.(scss)$/,
          use: [
              MiniCssExtractPlugin.loader,
              'css-loader',
              'postcss-loader',
              'sass-loader',
          ],
      },
    ],
  }
};
