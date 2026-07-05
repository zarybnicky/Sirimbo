import { serwist } from '@serwist/next/config';

// eslint-disable-next-line import-x/no-unused-modules
export default serwist.withNextConfig((nextConfig) => ({
  swSrc: 'sw.ts',
  swDest: 'public/sw.js',
  // disable: process.env.NODE_ENV !== 'production',
  globPatterns: [],
  precachePrerendered: false,
  maximumFileSizeToCacheInBytes: 1_000_000,
  globIgnores: [`${nextConfig.distDir}/server/pages/**/*.json`],
}));
