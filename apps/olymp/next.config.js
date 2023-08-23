const path = require('path');
const fs = require('fs');
const { execSync } = require('child_process');
const { tenantConfig, tenantAlias } = require('./tenant/config.js');

fs.symlinkSync(tenantAlias, './tenant/current.new');
fs.rename('./tenant/current.new', './tenant/current');

let withBundleAnalyzer = (x) => x;
if (process.env.ANALYZE === 'true') {
  withBundleAnalyzer = require('@next/bundle-analyzer')({ enabled: true });
}

let withSentryConfig = (x) => x;
if (process.env.NODE_ENV === 'production') {
  const sentry = require('@sentry/nextjs');
  withSentryConfig = cfg => sentry.withSentryConfig({
    ...cfg,
    sentry: {
      tunnelRoute: '/sentry',
      hideSourceMaps: true,
    },
  }, {
    silent: true,  // Suppresses all logs
  });
}

module.exports =
  withBundleAnalyzer(
    withSentryConfig({
      reactStrictMode: true,
      poweredByHeader: false,
      swcMinify: true,

      output: 'standalone',
      experimental: {
        outputFileTracingRoot: path.join(__dirname, '../../'),
        scrollRestoration: true,
      },
      transpilePackages: ['@app/graphql', '@app/map'],

      images: {
        domains: ['tkolymp.cz', 'www.tkolymp.cz', 'api.rozpisovnik.cz', 'files.rozpisovnik.cz'],
      },

      async redirects() {
        const redirects = [
          { source: '/home', destination: '/', permanent: true },
          { source: '/aktualne', destination: '/clanky', permanent: true },
          { source: '/aktualne/:path*', destination: '/clanky/:path*', permanent: true },
          { source: '/nopassword', destination: '/zapomenute-heslo', permanent: true },
          { source: '/register', destination: '/registrace', permanent: true },
          { source: '/oklubu/saly', destination: '/kde-trenujeme', permanent: true },
          { source: '/oklubu/klubovi-treneri', destination: '/treneri', permanent: true },
          { source: '/oklubu/externi-treneri', destination: '/treneri', permanent: true },
          { source: '/member', destination: '/dashboard', permanent: true },
          { source: '/member/akce', destination: '/akce', permanent: true },
          { source: '/member/rozpis', destination: '/rozpis', permanent: true },
          { source: '/member/nabidka', destination: '/rozpis', permanent: true },
          { source: '/member/treninky', destination: '/rozpis', permanent: true },
          { source: '/member/dokumenty', destination: '/dokumenty', permanent: true },
          { source: '/member/profil', destination: '/profil', permanent: true },
        ];

        if (!tenantConfig.enableHome) {
          redirects.push(
            { source: '/', destination: '/dashboard', permanent: false },
          )
        }
        if (!tenantConfig.enableArticles) {
          redirects.push(
            { source: '/clanky/:path*', destination: '/dashboard', permanent: false },
          )
        }

        return redirects;
      },

      async rewrites() {
        let rewrites = [];
        if (process.env.NODE_ENV !== 'production') {
          const graphqlUrl = process.env.GRAPHQL_BACKEND || 'http://localhost:4000';
          let phpUrl = process.env.NEXT_PUBLIC_BASE_URL;
          if (!phpUrl) {
            phpUrl = `http://${execSync('nixos-container show-ip olymp-test', {
              encoding: 'utf8',
            })}`;
          }
          rewrites.push(
            { source: '/member/download', destination: `${graphqlUrl}/member/download` },
            { source: '/galerie/:path*', destination: `${phpUrl}/galerie/:path*` },
            { source: '/graphql', destination: `${graphqlUrl}/graphql` },
            { source: '/graphqli', destination: `${graphqlUrl}/graphqli` },
            );
        } else {
          rewrites.push(
            { source: '/member/download', destination: `https://api.rozpisovnik.cz/member/download` },
            { source: '/galerie/:path*', destination: 'https://api.rozpisovnik.cz/galerie/:path*' },
            { source: "/ingest", destination: "https://eu.posthog.com" },
            { source: "/ingest/:path*", destination: "https://eu.posthog.com/:path*" },
          );
        }

        if (!tenantConfig.enableHome) {
          rewrites.push(
            { source: '/', destination: '/dashboard' },
          )
        }
        if (!tenantConfig.enableArticles) {
          rewrites.push(
            { source: '/clanky/:path*', destination: '/dashboard' },
          )
        }
        return rewrites;
      },

      webpack: function (config, { webpack, buildId }) {
        config.plugins.push(new webpack.DefinePlugin({
          'process.env.BUILD_ID': JSON.stringify(buildId),
          __SENTRY_DEBUG__: false,
          __SENTRY_TRACING__: false,
        }));
        return config;
      },
    }),
);
