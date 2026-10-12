import type { Metadata } from 'next';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { Crawler } from './Crawler';

export const metadata: Metadata = {
  title: 'Crawler',
  robots: { index: false, follow: false },
};

export default async function CrawlerPage() {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  return (
    <Layout requireSystemAdmin>
      <Crawler />
    </Layout>
  );
}
