import Link from 'next/link';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import { browseFederations } from './federated.queries';

export const metadata = { title: 'Federations', robots: { index: false, follow: false } };

export default async function FederationsPage() {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const federations = await runQuery(browseFederations);
  return (
    <Layout requireSystemAdmin>
      <PageHeader title="Federations" />
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['federation', 'name', 'events', 'competitions'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {federations.map((federation) => (
              <tr key={federation.code} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  <Link
                    className="text-accent-11 underline"
                    href={`/admin/federated/${federation.code}`}
                  >
                    {federation.code}
                  </Link>
                </th>
                <td className="px-2 py-1">{federation.name}</td>
                <td className="px-2 py-1">{federation.events}</td>
                <td className="px-2 py-1">{federation.competitions}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </Layout>
  );
}
