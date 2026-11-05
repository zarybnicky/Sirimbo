import type { Metadata } from 'next';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { Crawler } from './Crawler';
import { runQuery } from '@/lib/server/postgresql';
import { Histogram } from '@/ui/Histogram';
import { getRefetchOutlook } from './crawler.queries';

export const metadata: Metadata = {
  title: 'Crawler',
  robots: { index: false, follow: false },
};

export default async function CrawlerPage() {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const outlook = await runQuery(getRefetchOutlook);
  const overdue = outlook.find((row) => row.day === null)?.count ?? 0;
  return (
    <Layout requireSystemAdmin>
      <Crawler allowRefetch={process.env.CRAWLER_DISABLE_REFETCH !== 'true'}>
          <Histogram
            data={outlook.flatMap((row) =>
              row.day ? [[row.day, row.count] as const] : [],
            )}
            bucketWidth={36}
            formatLabel={(day) => day.slice(8) + '.' + day.slice(5, 7)}
          />
      </Crawler>
    </Layout>
  );
}
