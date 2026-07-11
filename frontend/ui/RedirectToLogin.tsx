'use client';

import * as React from 'react';

// Auth gate for views that require login: instead of embedding the login form
// (which, as a server-action form, cannot render under the Pages Router), send
// the user to the App Router /login page and let them return via ?from=.
export function RedirectToLogin({ from }: { from?: string }) {
  React.useEffect(() => {
    if (typeof window === 'undefined') return;
    const target = from ? `/login?from=${encodeURIComponent(from)}` : '/login';
    window.location.assign(target);
  }, [from]);
  return null;
}
