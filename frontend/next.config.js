//@ts-check

/** @type {any} */
const nextRoutes = require("nextjs-routes/config");
const withSerwist = require('@serwist/next').default({
  swSrc: 'serwist/sw.ts',
  swDest: 'public/sw.js',
  reloadOnOnline: false,
  disable: process.env.NODE_ENV !== 'production',
});

/** @type {(x: import('next').NextConfig) => import('next').NextConfig} */
let withBundleAnalyzer = (x) => x;
if (process.env.ANALYZE === 'true') {
  withBundleAnalyzer = require('@next/bundle-analyzer')({ enabled: true });
}

/** @type {(x: import('next').NextConfig) => import('next').NextConfig} */
const withSentryConfig = cfg => require('@sentry/nextjs').withSentryConfig(cfg, {
  tunnelRoute: '/sentry',
  widenClientFileUpload: true,
  silent: true,  // Suppresses all logs
});

/** @type {import('next').NextConfig} */
module.exports = withSerwist(
  nextRoutes()(
    withBundleAnalyzer(
      withSentryConfig({
        reactStrictMode: true,
        poweredByHeader: false,
        productionBrowserSourceMaps: true,
        devIndicators: false,

      output: 'standalone',
      experimental: {
        scrollRestoration: true,
      },

      images: {
        remotePatterns: [
          { protocol: 'https', hostname: 'api.rozpisovnik.cz' },
          { protocol: 'https', hostname: 'files.rozpisovnik.cz' },
        ],
        minimumCacheTTL: 2_678_400,
        unoptimized: true,
      },

      eslint: {
        ignoreDuringBuilds: true,
        dirs: [
          'pages',
          'graphql',
          'calendar',
          'map',
          'components',
          'ui',
          'editor',
          'lib',
        ],
      },

      async redirects() {
        /** @type {Awaited<ReturnType<NonNullable<import('next').NextConfig['redirects']>>>} */
        const redirects = [
          { source: '/home', destination: '/', permanent: true },
          { source: '/aktualne', destination: '/clanky', permanent: true },
          { source: '/aktualne/:path*', destination: '/clanky/:path*', permanent: true },
        ];

        const olympSpecificRedirects = [
          { source: '/camp', destination: '/clanky/468/letni-soustredeni-mohelnice', permanent: false },
          { source: '/mistrovstvi', destination: 'https://mistrovstvi.tkolymp.cz', permanent: true },
          { source: '/prijdtancit', destination: 'https://nabor.tkolymp.cz', permanent: true },
          { source: '/prijdtancit/deti', destination: 'https://nabor.tkolymp.cz', permanent: true },
          { source: '/prijdtancit/mladez', destination: 'https://nabor.tkolymp.cz', permanent: true },
        ];

        /** @type {Awaited<ReturnType<NonNullable<import('next').NextConfig['redirects']>>>[0]['has']} */
        const olympSpecificConditions = [
          { type: 'host', value: '(?<host>.*olymp.*)' },
          { type: 'cookie', key: 'tenant_id', value: '1' },
        ];

        for (const condition of olympSpecificConditions) {
          redirects.push(...olympSpecificRedirects.map(x => ({
            ...x,
            has: [condition],
          })))
        }

        return redirects;
      },

      async rewrites() {
        /** @type {Awaited<ReturnType<NonNullable<import('next').NextConfig['rewrites']>>>} */
        const rewrites = [
          {
            source: '/',
            destination: '/dashboard',
            missing: [{ type: 'cookie', key: 'tenant_id', value: '1' }],
          },
          {
            source: '/clanky/:path*',
            destination: '/dashboard',
            missing: [{ type: 'cookie', key: 'tenant_id', value: '1' },],
          },
        ];
        if (process.env.NODE_ENV !== 'production') {
          const graphqlServer = process.env.GRAPHQL_BACKEND || 'https://api.rozpisovnik.cz';
          const externalServer = process.env.EXTERNAL_SERVER_URL || graphqlServer;
          rewrites.push(
            { source: '/member/download', destination: `${graphqlServer}/member/download` },
            { source: '/galerie/:path*', destination: `${externalServer ?? ''}/galerie/:path*` },
            { source: '/graphql', destination: `${graphqlServer}/graphql` },
            { source: '/graphiql', destination: `${graphqlServer}/graphiql` },
          );
        } else {
          rewrites.push(
            { source: '/member/download', destination: "https://api.rozpisovnik.cz/member/download" },
            { source: '/galerie/:path*', destination: 'https://api.rozpisovnik.cz/galerie/:path*' },
          );
        }

        return rewrites;
      },


      webpack: (config, { webpack }) => {
        config.plugins.push(
          new webpack.DefinePlugin({
            __SENTRY_DEBUG__: false,
            __SENTRY_TRACING__: false,
          }),
        );
        return config;
      },
      }),
    ),
  ),
);
