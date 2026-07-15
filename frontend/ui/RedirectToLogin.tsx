'use client';

import * as React from 'react';
import { useAuthLoading } from '@/ui/use-auth';

// Auth gate for Pages-Router views: send the user to the App Router /login page.
// Waits for the auth query so a cookie-authenticated user with cold client
// state isn't bounced away before it resolves.
export function RedirectToLogin({ from }: { from?: string }) {
  const authLoading = useAuthLoading();
  React.useEffect(() => {
    if (authLoading) return;
    window.location.assign(from ? `/login?from=${encodeURIComponent(from)}` : '/login');
  }, [authLoading, from]);
  return null;
}
