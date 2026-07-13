/* eslint-disable import-x/no-unused-modules */
'use client';

import { StatusPage } from '@/ui/StatusPage';

export default function RootError({
  error: _error,
  reset,
}: {
  error: Error & { digest?: string };
  reset: () => void;
}) {
  return <StatusPage reset={reset} status="error" />;
}
