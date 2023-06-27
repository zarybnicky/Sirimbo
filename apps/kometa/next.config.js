const path = require('path');

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
        domains: ['tkolymp.cz', 'www.tkolymp.cz', 'api.rozpisovnik.cz', 'files.rozpisovnik.cz'],
      },

      async redirects() {
        return [
          { source: '/home', destination: '/', permanent: true },
        ];
      },

      async rewrites() {
        if (process.env.NODE_ENV !== 'production') {
          const graphqlUrl = process.env.GRAPHQL_BACKEND || 'http://localhost:4000';
          return [
            { source: '/graphql', destination: `${graphqlUrl}/graphql` },
            { source: '/graphqli', destination: `${graphqlUrl}/graphqli` },
          ];
        } else {
          return [
            { source: "/ingest/:path*", destination: "https://eu.posthog.com/:path*" },
          ];
        }
      },

      webpack: function (config, { webpack, buildId }) {
        const defines = {
          'process.env.BUILD_ID': JSON.stringify(buildId),
        };
        config.plugins.push(new webpack.DefinePlugin(defines));
        return config;
      },
    }),
  ),
);
