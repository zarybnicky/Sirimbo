import { Table } from '@/ui/Table';
import Link from 'next/link';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationListDocument } from '@/graphql/Federation';

export const metadata = { title: 'Soutěžní data', robots: { index: false, follow: false } };

export default async function FederationsPage() {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federationsList: federations } = await executeGraphql(FederationListDocument);
  return (
    <Layout requireUser>
      <PageHeader title="Soutěžní data" />
      <Table
        columns={[
          'Organizace',
          'Soutěže',
          'Osoby',
          'Soutěžící',
        ]}
      >
        {federations?.map((federation) => (
          <tr key={federation.code} className="border-t border-neutral-4">
            <th scope="row">
              <Link
                className="text-accent-11 underline"
                href={`/federated/${federation.code}`}
              >
                {federation.name}
              </Link>
            </th>
            <td>{federation.events?.totalCount}</td>
            <td>{federation.people?.totalCount}</td>
            <td>{federation.competitors?.totalCount}</td>
          </tr>
        ))}
      </Table>
    </Layout>
  );
}
