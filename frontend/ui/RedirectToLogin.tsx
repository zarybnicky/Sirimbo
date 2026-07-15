'use client';

import * as React from 'react';

// Auth gate for Pages-Router views: send the user to the App Router /login page.
export function RedirectToLogin({ from }: { from?: string }) {
  React.useEffect(() => {
    window.location.assign(from ? `/login?from=${encodeURIComponent(from)}` : '/login');
  }, [from]);
  return null;
}
