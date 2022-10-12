const path = require('path');
const { execSync } = require('child_process');

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
      { source: '/member/rozpis', destination: '/schedule', permanent: true },
      { source: '/member/nabidka', destination: '/schedule', permanent: true },
      { source: '/member/treninky', destination: '/schedule', permanent: true },
      { source: '/member/dokumenty', destination: '/documents', permanent: true },
    ];
  },

  async rewrites() {
    const graphqlUrl = process.env.GRAPHQL_BACKEND || 'http://localhost:4000';
    let phpUrl = process.env.NEXT_PUBLIC_BASE_URL;
    if (!phpUrl) {
      phpUrl = `http://${execSync('nixos-container show-ip olymp-test', {encoding: 'utf8'})}`;
    }
    return [
      { source: '/graphql', destination: `${graphqlUrl}/graphql` },
      { source: '/graphqli', destination: `${graphqlUrl}/graphqli` },
      { source: '/old/:path*', destination: `${phpUrl}/old/:path*` },
      { source: '/galerie/:path*', destination: `${phpUrl}/galerie/:path*` },
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
