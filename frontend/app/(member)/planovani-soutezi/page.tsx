import type { Metadata } from 'next';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { getRequestContext } from '@/lib/server/tenant';
import { CompetitionPlanning } from './CompetitionPlanning';

export const metadata: Metadata = {
  title: 'Plánování soutěží',
  robots: { index: false, follow: false },
};

export default async function CompetitionPlanningPage() {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  return (
    <Layout requireUser>
      <PageHeader title="Plánování soutěží" />
      <CompetitionPlanning />
    </Layout>
  );
}
