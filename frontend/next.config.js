const path = require('path');
const { execSync } = require('child_process');

let withBundleAnalyzer = (x) => x;
if (process.env.ANALYZE === 'true') {
  withBundleAnalyzer = require('@next/bundle-analyzer')({ enabled: true });
}

let withSentryConfig = (x) => x;
if (process.env.NODE_ENV === 'production') {
  withSentryConfig = cfg => require('@sentry/nextjs').withSentryConfig({
    ...cfg,
    sentry: {
      tunnelRoute: '/sentry',
      hideSourceMaps: true,
    },
  }, {
    silent: true,  // Suppresses all logs
  });
}

module.exports = require('nextjs-routes/config')({ outDir: '.' })(
  withBundleAnalyzer(
    withSentryConfig({
      reactStrictMode: true,
      poweredByHeader: false,
      swcMinify: true,

      output: 'standalone',
      experimental: {
        outputFileTracingRoot: path.join(__dirname, '../'),
      },
      transpilePackages: ['@app/graphql'],

      images: {
        domains: ['tkolymp.cz', 'www.tkolymp.cz'],
      },

      async redirects() {
        return [
          { source: '/home', destination: '/', permanent: true },
          { source: '/aktualne', destination: '/articles', permanent: true },
          { source: '/aktualne/:path*', destination: '/articles/:path*', permanent: true },
          { source: '/nopassword', destination: '/forgotten-password', permanent: true },
          { source: '/registrace', destination: '/register', permanent: true },
          { source: '/kontakt', destination: '/contact', permanent: true },
          { source: '/oklubu/saly', destination: '/kde-trenujeme', permanent: true },
          { source: '/oklubu/klubovi-treneri', destination: '/treneri', permanent: true },
          { source: '/oklubu/externi-treneri', destination: '/treneri', permanent: true },
          { source: '/member', destination: '/dashboard', permanent: true },
          { source: '/member/akce', destination: '/events', permanent: true },
          { source: '/member/rozpis', destination: '/schedule', permanent: true },
          { source: '/member/rozpis', destination: '/schedule', permanent: true },
          { source: '/member/nabidka', destination: '/schedule', permanent: true },
          { source: '/member/treninky', destination: '/schedule', permanent: true },
          { source: '/member/dokumenty', destination: '/documents', permanent: true },
          { source: '/member/profil', destination: '/profile', permanent: true },
        ];
      },

      async rewrites() {
        if (process.env.NODE_ENV !== 'production') {
          const graphqlUrl = process.env.GRAPHQL_BACKEND || 'http://localhost:4000';
          let phpUrl = process.env.NEXT_PUBLIC_BASE_URL;
          if (!phpUrl) {
            phpUrl = `http://${execSync('nixos-container show-ip olymp-test', {
              encoding: 'utf8',
            })}`;
          }
          return [
            { source: '/member/download', destination: `${graphqlUrl}/member/download` },
            { source: '/graphql', destination: `${graphqlUrl}/graphql` },
            { source: '/graphqli', destination: `${graphqlUrl}/graphqli` },
            { source: '/galerie/:path*', destination: `${phpUrl}/galerie/:path*` },
          ];
        } else {
          return [
            { source: '/galerie/:path*', destination: 'https://api.rozpisovnik.cz/galerie/:path*' },
            { source: "/ingest/:path*", destination: "https://eu.posthog.com/:path*" },
          ];
        }
      },

      webpack: function (config, { isServer, webpack, buildId }) {
        const defines = {
          'process.env.BUILD_ID': JSON.stringify(buildId),
        };
        config.plugins.push(new webpack.DefinePlugin(defines));
        return config;
      },
    }),
  ),
);
