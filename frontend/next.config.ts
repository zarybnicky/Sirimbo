import nextRoutes from "nextjs-routes/config";
import serwistNext from '@serwist/next';
import bundleAnalyzer from '@next/bundle-analyzer';
import { withSentryConfig } from '@sentry/nextjs';

type NextConfig = import('next').NextConfig;
type NextPlugin = (config: NextConfig) => NextConfig;
type NextRedirect = Awaited<ReturnType<NonNullable<NextConfig['redirects']>>>;
type NextRewrite = Awaited<ReturnType<NonNullable<NextConfig['rewrite']>>>;

const compose = (...plugins: NextPlugin[]) => (config: NextConfig): NextConfig =>
  plugins.reduceRight((acc, fn) => fn(acc), config);

// eslint-disable-next-line import/no-unused-modules
export default compose(
  serwistNext({
    swSrc: 'sw.ts',
    swDest: 'public/sw.js',
    reloadOnOnline: false,
    // disable: process.env.NODE_ENV !== 'production',
    maximumFileSizeToCacheInBytes: 1_000_000,
  }),
  nextRoutes(),
  bundleAnalyzer({ enabled: process.env.ANALYZE === 'true' }),
  (cfg: NextConfig) => withSentryConfig(cfg, {
    tunnelRoute: '/sentry',
    widenClientFileUpload: true,
    silent: true,  // Suppresses all logs
  }),
)({
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
    qualities: [75, 90],
  },

  eslint: {
    ignoreDuringBuilds: true,
    dirs: [
      'pages',
      'graphql',
      'calendar',
      'map',
      'ui',
      'editor',
      'lib',
    ],
  },

  async redirects() {
    const redirects: NextRedirect = [
      { source: '/home', destination: '/', permanent: true },
      { source: '/aktualne', destination: '/clanky', permanent: true },
      { source: '/aktualne/:path*', destination: '/clanky/:path*', permanent: true },
    ];
    const olympSpecificRedirects = [
      { source: '/tancirna', destination: '/clanky/470/olymp-dance-night-2026', permanent: false },
      { source: '/camp', destination: '/clanky/468/letni-soustredeni-mohelnice', permanent: false },
      { source: '/mistrovstvi', destination: 'https://mistrovstvi.tkolymp.cz', permanent: true },
      { source: '/prijdtancit', destination: 'https://nabor.tkolymp.cz', permanent: true },
      { source: '/prijdtancit/deti', destination: 'https://nabor.tkolymp.cz', permanent: true },
      { source: '/prijdtancit/mladez', destination: 'https://nabor.tkolymp.cz', permanent: true },
    ];

    const olympSpecificConditions: NextRedirect[0]['has'] = [
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
    const rewrites: NextRewrite = [
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
});
