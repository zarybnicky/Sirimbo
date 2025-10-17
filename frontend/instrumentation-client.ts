import { init, captureRouterTransitionStart } from '@sentry/nextjs';

const SENTRY_DSN = process.env.SENTRY_DSN || process.env.NEXT_PUBLIC_SENTRY_DSN;

if (process.env.NODE_ENV === "production") {
  init({
    environment: process.env.NEXT_PUBLIC_SENTRY_ENVIRONMENT || 'development',
    ignoreErrors: [
      /ResizeObserver loop/,
      /attempted to hard navigate to the same URL/,
    ],
    dsn: SENTRY_DSN || 'https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825',
    tracesSampleRate: 1,
    sendDefaultPii: true,
  });
}

// eslint-disable-next-line import/no-unused-modules
export const onRouterTransitionStart = async (...args: [any, any]) => {
  if (process.env.NODE_ENV === "production") {
    captureRouterTransitionStart(...args);
  }
};
