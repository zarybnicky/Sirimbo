const path = require("path");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const webpack = require("webpack");
const TerserWebpackPlugin = require("terser-webpack-plugin");
const OptimizeCssAssetsPlugin = require("optimize-css-assets-webpack-plugin");
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');

module.exports = function(_env, argv) {
  const isProduction = argv.mode === "production";
  const isDevelopment = !isProduction;

  return {
    devtool: isDevelopment && "cheap-module-source-map",
    entry: "./src/index.jsx",
    output: {
      path: path.resolve(__dirname, "dist"),
      filename: "assets/js/[name].[contenthash:8].js",
      publicPath: "/"
    },
    module: {
      rules: [
        {
          test: /\.[tj]sx?$/,
          exclude: /node_modules/,
          use: {
            loader: 'ts-loader',
            options: { transpileOnly: true },
          },
        },
        {
          test: /\.css$/,
          use: [
            isProduction ? MiniCssExtractPlugin.loader : "style-loader",
            "css-loader"
          ]
        },
        {
          test: /\.scss$/,
          use: [
            isProduction ? MiniCssExtractPlugin.loader : "style-loader",
              "css-loader",
            {
              loader: "sass-loader",
              options: { prependData: `
@import '@wordpress/base-styles/_colors.scss';
@import '@wordpress/base-styles/_variables.scss';
@import '@wordpress/base-styles/_mixins.scss';
@import '@wordpress/base-styles/_breakpoints.scss';
@import '@wordpress/base-styles/_animations.scss';
@import '@wordpress/base-styles/_z-index.scss';` },
            },
          ]
        },
        {
          test: /\.(png|jpg|gif)$/i,
          use: {
            loader: "url-loader",
            options: {
              limit: 8192,
              name: "static/media/[name].[hash:8].[ext]"
            }
          }
        },
        {
          test: /\.svg$/,
          use: ["@svgr/webpack"]
        },
        {
          test: /\.(eot|otf|ttf|woff|woff2)$/,
          loader: require.resolve("file-loader"),
          options: {
            name: "static/media/[name].[hash:8].[ext]"
          }
        }
      ]
    },
    resolve: {
      extensions: [".js", ".jsx", '.ts', '.tsx'],
    },
    plugins: [
      isProduction &&
        new MiniCssExtractPlugin({
          filename: "assets/css/[name].[contenthash:8].css",
          chunkFilename: "assets/css/[name].[contenthash:8].chunk.css"
        }),
      new webpack.ProvidePlugin({
        'React': 'react',
        'ReactDOM': 'react-dom',
        '_': 'lodash',
        'moment': 'moment',
      }),
      new ForkTsCheckerWebpackPlugin({
        async: false,
        useTypescriptIncrementalApi: true,
        memoryLimit: 4096
      }),
      new HtmlWebpackPlugin({
        template: path.resolve(__dirname, "src/index.html"),
        inject: true
      }),
      new webpack.DefinePlugin({
        "process.env.NODE_ENV": JSON.stringify(
          isProduction ? "production" : "development"
        )
      })
    ].filter(Boolean),
    optimization: {
      minimize: isProduction,
      minimizer: [
        new TerserWebpackPlugin({
          terserOptions: {
            compress: {
              comparisons: false
            },
            mangle: {
              safari10: true
            },
            output: {
              comments: false,
              ascii_only: true
            },
            warnings: false
          }
        }),
        new OptimizeCssAssetsPlugin()
      ],
      splitChunks: {
        chunks: "all",
        minSize: 0,
        maxInitialRequests: 4,
        maxAsyncRequests: 4,
        cacheGroups: {
          vendors: {
            test: /[\\/]node_modules[\\/]/,
            name(module, chunks, cacheGroupKey) {
              const packageName = module.context.match(
                /[\\/]node_modules[\\/](.*?)([\\/]|$)/
              )[1];
              return `${cacheGroupKey}.${packageName.replace("@", "")}`;
            }
          },
          common: {
            minChunks: 2,
            priority: -10
          }
        }
      },
      runtimeChunk: "single"
    },
    devServer: {
      compress: true,
      historyApiFallback: true,
      open: true,
      overlay: true,
      port: 4200,
      proxy: {
        '/wp': 'http://localhost:4000'
      }
    }
  };
};
