import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { redirect } from 'next/navigation';
import { Dashboard } from './Dashboard';

export const metadata: Metadata = {
  title: 'Nástěnka',
  robots: { index: false, follow: false },
};

export default async function DashboardPage() {
  const { claims } = await getRequestContext();
  if (claims?.my_person_ids.length === 0) redirect('/profil');

  return (
    <Layout requireMember className="grow content relative content-stretch">
      <Dashboard />
    </Layout>
  );
}
