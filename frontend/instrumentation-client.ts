import * as Sentry from '@sentry/nextjs';

const SENTRY_DSN = process.env.SENTRY_DSN || process.env.NEXT_PUBLIC_SENTRY_DSN;

Sentry.init({
  environment: process.env.NEXT_PUBLIC_SENTRY_ENVIRONMENT || 'development',
  ignoreErrors: [
    /ResizeObserver loop/,
    /attempted to hard navigate to the same URL/,
  ],
  dsn: SENTRY_DSN || 'https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825',
  tracesSampleRate: 1,
  sendDefaultPii: true,
});
