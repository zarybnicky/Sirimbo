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
    config.module.rules = [
      ...config.module.rules.map(rule => {
        if (/svg/.test(rule.test)) {
          return { ...rule, exclude: /\.svg$/i }
        }
        return rule;
      }),
      {
        test: /\.svg$/i,
        use: ["@svgr/webpack"]
      },
      {
        test: /\.scss$/,
        use: ['style-loader', 'css-loader', 'resolve-url-loader', 'sass-loader'],
        include: path.resolve(__dirname, '../'),
      }
    ];
    return config;
  },
}
