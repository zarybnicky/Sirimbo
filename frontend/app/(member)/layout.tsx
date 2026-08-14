/* eslint-disable import-x/no-unused-modules */
import { getRequestState } from '@/lib/server/request-state';
import { Providers } from '@/ui/Providers';
import type { ReactNode } from 'react';

export default async function MemberLayout({ children }: { children: ReactNode }) {
  const { tenant, auth } = await getRequestState();

  return (
    <Providers initialAuth={auth} initialTenantId={tenant.id}>
      {children}
    </Providers>
  );
}
