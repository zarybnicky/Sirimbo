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

const federationColors: Record<string, string> = {
  csts: 'fill-accent-9',
  wdsf: 'fill-green-9',
  szts: 'fill-danger-9',
};

export default async function CrawlerPage() {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const outlook = await runQuery(getRefetchOutlook);
  const days = Map.groupBy(outlook, (row) => row.day);
  const federations = [
    ...new Set(
      outlook
        .filter((row) => row.day !== null)
        .map((row) => row.federation)
        .filter((value) => value !== null),
    ),
  ].toSorted();
  return (
    <Layout requireSystemAdmin>
      <Crawler allowRefetch={process.env.CRAWLER_DISABLE_REFETCH !== 'true'}>
        <div className="mb-2 flex flex-wrap gap-3 text-xs text-neutral-11">
          {federations.map((federation) => (
            <span key={federation} className="inline-flex items-center gap-1">
              <svg width="12" height="12" aria-hidden="true">
                <rect
                  width="12"
                  height="12"
                  className={federationColors[federation] ?? 'fill-neutral-9'}
                />
              </svg>
              {federation}
            </span>
          ))}
        </div>
        <Histogram
          data={[...days].flatMap(([day, rows]) =>
            day
              ? [
                  [
                    day,
                    rows.reduce((sum, row) => sum + row.count, 0),
                    rows.flatMap((row) =>
                      row.federation
                        ? [
                            {
                              label: row.federation,
                              count: row.count,
                              className:
                                federationColors[row.federation] ?? 'fill-neutral-9',
                            },
                          ]
                        : [],
                    ),
                  ] as const,
                ]
              : [],
          )}
          bucketWidth={36}
          formatLabel={(day) => day.slice(8) + '.' + day.slice(5, 7)}
        />
      </Crawler>
    </Layout>
  );
}
