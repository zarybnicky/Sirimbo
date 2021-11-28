const path = require('path');

module.exports = {
  "core": {
    "builder": 'webpack5',
  },
  reactOptions: {
    fastRefresh: true,
  },
  "stories": [
    "../src/**/*.stories.mdx",
    "../src/**/*.stories.@(js|jsx|ts|tsx)"
  ],
  "addons": [
    "@storybook/addon-links",
    "@storybook/addon-postcss",
    "@storybook/preset-scss",
    "@storybook/addon-essentials",
    '@storybook/addon-a11y'
  ],
  webpackFinal: async (config, { configType }) => {
    config.module.rules.push({
      test: /\.(png|woff|woff2|eot|ttf|svg)$/,
      use: ['file-loader'],
      include: path.resolve(__dirname, '../')
    });
    config.module.rules.push({
      test: /\.scss$/,
      use: ['style-loader', 'css-loader', 'resolve-url-loader', 'sass-loader'],
      include: path.resolve(__dirname, '../'),
    });
    /* config.plugins.push(new MiniCssExtractPlugin({
     *   filename: '[name]-[contenthash].css',
     *   chunkFilename: '[id]-[contenthash].css',
     * })); */
    return config;
  },
}
