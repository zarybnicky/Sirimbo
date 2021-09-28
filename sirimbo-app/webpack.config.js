const path = require("path");
const fs = require("fs");
const webpack  = require('webpack');
const CopyPlugin = require("copy-webpack-plugin");
const ExtractCssChunks = require('extract-css-chunks-webpack-plugin');

module.exports = {
  mode: 'development',
  entry: './src/index.ts',
  output: {
      filename: 'main.js',
      publicPath: '/',
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx', '.json']
  },
  devtool: "source-map",
  devServer: {
    contentBase: './',
  },
  plugins: [
      new ExtractCssChunks(),
      new CopyPlugin({ patterns: [{ from: "static" }] }),
      new webpack.ProvidePlugin({
          $: 'jquery',
          jQuery: 'jquery',
          'window.jQuery': 'jquery',
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
              ExtractCssChunks.loader,
              'css-loader',
              'postcss-loader',
              'sass-loader'
          ],
      },
      {
          test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
          type: 'asset/resource',
          generator: {
              filename: 'fonts/[name][ext][query]',
          },
      },
      {
          test: /\.(jpe?g|png|gif|svg)$/i,
          type: 'asset/resource',
          generator: {
              filename: 'icons/[name][ext][query]',
          },
      },
    ],
  }
};
