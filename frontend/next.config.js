const path = require('path');

module.exports = {
  reactStrictMode: true,
  poweredByHeader: false,
  swcMinify: true,

  output: 'standalone',
  experimental: {
    outputFileTracingRoot: path.join(__dirname, '../'),
  },

  async redirects() {
    return [
      { source: '/home', destination: '/', permanent: true },
    ];
  },

  async rewrites() {
    return [
      { source: '/graphql', destination: 'http://localhost:4000' },
      { source: '/graphiql', destination: 'http://localhost:4000' },
      { source: '/old', destination: 'http://olymp-test' },
      { source: '/galerie', destination: 'http://olymp-test' },
    ];
  },

  webpack: function (config, { isServer, webpack, buildId }) {
    config.plugins.push(
      new webpack.DefinePlugin({
        'process.env.BUILD_ID': JSON.stringify(buildId)
      })
    );

    config.module.rules.push({
      test: /\.(svg)$/,
      include: path.resolve(__dirname, 'public'),
      loader: 'svg-react-loader',
    });

    return config;
  },
};
