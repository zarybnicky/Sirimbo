import bundleAnalyzer from '@next/bundle-analyzer';
import { withSentryConfig } from '@sentry/nextjs/config';
import path from "node:path";
import { imageWidths } from './lib/image-presets.ts';

type NextConfig = import('next').NextConfig;
type NextPlugin = (config: NextConfig) => NextConfig;
type NextRedirect = Awaited<ReturnType<NonNullable<NextConfig['redirects']>>>;

const compose =
  (...plugins: NextPlugin[]) =>
  (config: NextConfig): NextConfig =>
    plugins.reduceRight((acc, fn) => fn(acc), config);

export default compose(
  bundleAnalyzer({ enabled: process.env.ANALYZE === 'true' }),
  (cfg: NextConfig) =>
    withSentryConfig(cfg, {
      widenClientFileUpload: true,
      tunnelRoute: '/tattletale',
      org: 'jakub-zarybnicky',
      project: 'olymp',
      authToken: process.env.SENTRY_AUTH_TOKEN,
      bundleSizeOptimizations: {
        excludeReplayShadowDom: true,
      },
      sourcemaps: {
        deleteSourcemapsAfterUpload: true,
      },
      telemetry: false,
    }),
)({
  reactStrictMode: true,
  poweredByHeader: false,
  productionBrowserSourceMaps: true,
  devIndicators: {
    position: 'bottom-left',
  },
  typedRoutes: true,
  transpilePackages: ['html-react-parser', '@rozpisovnik/graphql'],
  serverExternalPackages: ['postgraphile', 'grafast', 'grafserv', 'graphile-build', 'graphile-build-pg', 'graphile-config', '@dataplan/pg', 'pg-sql2', 'graphql'],
  agentRules: false,

  output: process.env.VERCEL ? undefined : 'standalone',
  bundlePagesRouterDependencies: true,
  experimental: {
    scrollRestoration: true,
    turbopackGc: true,
    agentFeedback: false,
    agentUpgrade: false,
    turbopackPluginRuntimeStrategy: 'workerThreads',
 },
  pageExtensions: ['js', 'jsx', 'ts', 'tsx'],
  outputFileTracingRoot: path.join(import.meta.dirname, '..'),
  outputFileTracingIncludes: {
    "/*": [
      "node_modules/sharp/**/*",
      "node_modules/@img/**/*",
    ],
  },

  images: {
    imageSizes: imageWidths.filter((width) => width < 512),
    deviceSizes: imageWidths.filter((width) => width >= 512),
    contentDispositionType: 'inline',
    remotePatterns: [
      { protocol: 'https', hostname: 'api.rozpisovnik.cz' },
      { protocol: 'https', hostname: 'tkolymp.cz' },
    ],
    formats: ['image/webp'],
    minimumCacheTTL: 2_678_400,
    maximumDiskCacheSize: 512 * 1024 * 1024,
    qualities: [75],
  },

  async redirects() {
    const redirects: NextRedirect = [
      { source: '/graphiql', destination: '/graphql', permanent: false },
      { source: '/home', destination: '/', permanent: true },
      { source: '/aktualne', destination: '/clanky', permanent: true },
      { source: '/aktualne/:path*', destination: '/clanky/:path*', permanent: true },
      {
        source: '/akce/:id/termin/:instance',
        destination: '/termin/:instance',
        permanent: true,
      },
    ];
    const olympSpecificRedirects = [
      {
        source: '/soutez',
        destination: '/clanky/473/olymp-prostejov-cup-2026',
        permanent: false,
      },
      {
        source: '/tancirna',
        destination: '/clanky/475/olymp-dance-night-vol-2',
        permanent: false,
      },
      {
        source: '/camp',
        destination: '/clanky/468/letni-soustredeni-mohelnice',
        permanent: false,
      },
      {
        source: '/mistrovstvi',
        destination: 'https://mistrovstvi.tkolymp.cz',
        permanent: true,
      },
      {
        source: '/prijdtancit',
        destination: 'https://nabor.tkolymp.cz',
        permanent: true,
      },
      {
        source: '/prijdtancit/deti',
        destination: 'https://nabor.tkolymp.cz',
        permanent: true,
      },
      {
        source: '/prijdtancit/mladez',
        destination: 'https://nabor.tkolymp.cz',
        permanent: true,
      },
    ];

    const olympSpecificConditions: NextRedirect[0]['has'] = [
      { type: 'host', value: '(?<host>.*olymp.*)' },
      { type: 'cookie', key: 'tenant_id', value: '1' },
    ];

    for (const condition of olympSpecificConditions) {
      redirects.push(
        ...olympSpecificRedirects.map((x) => ({
          ...x,
          has: [condition],
        })),
      );
    }
    return redirects;
  },

  async rewrites() {
    return [
      { source: '/galerie/:path*', destination: 'https://api.rozpisovnik.cz/gallery/:path*' },
    ];
  },
});
