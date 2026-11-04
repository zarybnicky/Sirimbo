import type { Metadata } from 'next';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { CrawlerJobs } from '../Crawler';

export const metadata: Metadata = {
  title: 'Crawler jobs',
  robots: { index: false, follow: false },
};

export default async function CrawlerJobsPage() {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  return (
    <Layout requireSystemAdmin>
      <CrawlerJobs />
    </Layout>
  );
}
