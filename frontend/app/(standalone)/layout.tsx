/* eslint-disable import-x/no-unused-modules */
import { getRequestState } from '@/lib/server/request-state';
import { Providers } from '@/ui/Providers';
import type { Metadata } from 'next';
import type { ReactNode } from 'react';

export const metadata: Metadata = {
  robots: {
    index: false,
    follow: false,
  },
};

export default async function StandaloneLayout({ children }: { children: ReactNode }) {
  const { tenant, auth } = await getRequestState();

  return (
    <Providers initialAuth={auth} initialTenantId={tenant.id}>
      {children}
    </Providers>
  );
}
