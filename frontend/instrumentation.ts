import { init, captureRequestError } from '@sentry/nextjs';

// eslint-disable-next-line import/no-unused-modules
export async function register() {
  if (process.env.NODE_ENV === "production") {
    const SENTRY_DSN = process.env.SENTRY_DSN || process.env.NEXT_PUBLIC_SENTRY_DSN;
    init({
      environment: process.env.NEXT_PUBLIC_SENTRY_ENVIRONMENT || process.env.NODE_ENV,
      ignoreErrors: [
        /ResizeObserver loop completed with undelivered notifications/,
        /attempted to hard navigate to the same URL/,
      ],
      dsn: SENTRY_DSN || 'https://943ee3e7e7044524b2ee8413a957e14f@o775093.ingest.sentry.io/5796825',
      tracesSampleRate: 1,
    });
  }
}

// eslint-disable-next-line import/no-unused-modules
export const onRequestError = async (...args: [any, any, any]) => {
  if (process.env.NODE_ENV === "production") {
    captureRequestError(...args);
  }
};
