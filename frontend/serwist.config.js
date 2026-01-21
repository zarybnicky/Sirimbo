import { serwist } from '@serwist/next/config';

// eslint-disable-next-line import/no-unused-modules
export default serwist.withNextConfig((nextConfig) => ({
  swSrc: 'sw.ts',
  swDest: 'public/sw.js',
  // disable: process.env.NODE_ENV !== 'production',
  maximumFileSizeToCacheInBytes: 1_000_000,
  globIgnores: [`${nextConfig.distDir}/server/pages/**/*.json`],
}));
