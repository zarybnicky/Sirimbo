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
    "@storybook/addon-essentials",
    '@storybook/addon-a11y'
  ]
}
