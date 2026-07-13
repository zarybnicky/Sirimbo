'use client';

import { StatusPage } from '@/ui/StatusPage';

/* eslint-disable import-x/no-unused-modules */
export default function StandaloneError({
  error: _error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  return <StatusPage reset={reset} status="error" />;
}
