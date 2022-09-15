const path = require('path');

let withBundleAnalyzer = x => x;
if (process.env.ANALYZE === 'true') {
  withBundleAnalyzer = require('@next/bundle-analyzer')({ enabled: true });
}

let withSentryConfig = x => x;
if (process.env.NODE_ENV === 'production') {
  withSentryConfig = cfg => require('@sentry/nextjs').withSentryConfig({
    ...cfg,
    sentry: {
      hideSourceMaps: true,
    },
  }, {
    silent: true, // Suppresses all logs
  });
}

module.exports = withBundleAnalyzer(withSentryConfig({
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
      { source: '/nopassword', destination: '/forgotten-password', permanent: true },
      { source: '/registrace', destination: '/register', permanent: true },
      { source: '/kontakt', destination: '/contact', permanent: true },
      { source: '/oklubu/saly', destination: '/o-nas/kde-trenujeme', permanent: true },
      { source: '/oklubu/klubovi-treneri', destination: '/o-nas/treneri', permanent: true },
      { source: '/oklubu/externi-treneri', destination: '/o-nas/treneri', permanent: true },
      { source: '/member', destination: '/dashboard', permanent: true },
      { source: '/member/home', destination: '/dashboard', permanent: true },
    ];
  },

  async rewrites() {
    return [
      { source: '/graphql', destination: 'http://localhost:4000/graphql' },
      { source: '/graphiql', destination: 'http://localhost:4000/graphiql' },
      { source: '/old/:path*', destination: 'http://olymp-test/:path*' },
      { source: '/galerie/:path*', destination: 'http://olymp-test/galerie/:path*' },
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
}));
