/* eslint-disable import-x/no-unused-modules */
'use client';

import { captureException } from '@sentry/nextjs';
import { StatusPage } from '@/ui/StatusPage';
import { useEffect } from 'react';

export default function GlobalError({
  error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  useEffect(() => {
    captureException(error);
  }, [error]);

  return (
    <html lang="cs">
      <body>
        <StatusPage reset={reset} status="error" />
      </body>
    </html>
  );
}
