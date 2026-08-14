/* eslint-disable import-x/no-unused-modules */
import { getRequestState } from '@/lib/server/request-state';
import { Layout } from '@/ui/Layout';
import { Providers } from '@/ui/Providers';
import { StatusPage } from '@/ui/StatusPage';

export default async function NotFound() {
  const { tenant, auth } = await getRequestState();

  return (
    <Providers initialAuth={auth} initialTenantId={tenant.id}>
      <Layout hideTopMenuIfLoggedIn>
        <StatusPage status="not-found" />
      </Layout>
    </Providers>
  );
}
