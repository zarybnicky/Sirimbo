/* eslint-disable import-x/no-unused-modules */
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { buildId } from '@/lib/build-id';
import { executeGraphql } from '@/lib/server/graphql';
import { getRequestTenant } from '@/tenant/server';
import { Providers } from '@/ui/Providers';
import type { SessionClaims } from '@/ui/state/auth';
import type { ReactNode } from 'react';

export default async function MemberLayout({ children }: { children: ReactNode }) {
  const [tenant, data] = await Promise.all([
    getRequestTenant(),
    executeGraphql(CurrentUserDocument, { versionId: buildId }),
  ]);
  const claims =
    typeof data.currentClaims === 'string'
      ? (JSON.parse(data.currentClaims) as SessionClaims)
      : (data.currentClaims as SessionClaims | null);

  return (
    <Providers
      initialAuth={{
        tenantId: tenant.id,
        claims,
        user: data.getCurrentUser,
      }}
    >
      {children}
    </Providers>
  );
}
