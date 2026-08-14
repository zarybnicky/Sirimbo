'use client';

import { captureException } from '@sentry/nextjs';
import { StatusPage } from '@/ui/StatusPage';
import { useEffect } from 'react';

/* eslint-disable import-x/no-unused-modules */
export default function PublicError({
  error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  useEffect(() => {
    captureException(error);
  }, [error]);

  return <StatusPage reset={reset} status="error" />;
}
