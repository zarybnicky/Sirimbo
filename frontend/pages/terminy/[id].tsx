import { EventSeriesDocument } from '@/graphql/Event';
import { cn } from '@/lib/cn';
import { Layout } from '@/ui/Layout';
import { Spinner } from '@/ui/Spinner';
import { PageHeader } from '@/ui/TitleBar';
import { FormError } from '@/ui/form';
import { formatEventType, fullDateFormatter } from '@/ui/format';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { NextSeo } from 'next-seo';
import Link from 'next/link';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export default function EventSeriesPage() {
  const {
    query: { id },
  } = useTypedRouter(QueryParams);
  const [{ data, error, fetching }] = useQuery({
    query: EventSeriesDocument,
    variables: { id },
    pause: !id,
  });
  const series = data?.eventSeries;
  const title = series?.name || 'Termíny';

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={title} />
      <div className="col-feature min-h-[60vh] p-4 lg:pb-8">
        <PageHeader title={title} />
        <FormError error={error} />
        {fetching && !series ? <Spinner /> : null}
        {!fetching && !series ? <p>Série termínů nebyla nalezena.</p> : null}
        {series?.eventsList.length === 0 ? <p>Série nemá žádné termíny.</p> : null}
        {series?.eventsList.length ? (
          <div className="divide-y divide-neutral-4 rounded-lg border border-neutral-4 bg-neutral-1">
            {series.eventsList.map((instance) => {
              const location = instance.location?.name || instance.locationText;
              return (
                <Link
                  key={instance.id}
                  href={`/termin/${instance.id}`}
                  className="grid gap-1 p-3 hover:bg-neutral-2 sm:grid-cols-[minmax(0,1fr)_auto]"
                >
                  <div
                    className={cn(
                      'font-medium text-neutral-12',
                      instance.isCancelled && 'line-through text-neutral-10',
                    )}
                  >
                    {instance.name || formatEventType(instance.type)}
                  </div>
                  <div className="text-sm tabular-nums text-neutral-11 sm:text-right">
                    {fullDateFormatter.formatRange(
                      new Date(instance.since),
                      new Date(instance.until),
                    )}
                  </div>
                  {location ? (
                    <div className="text-sm text-neutral-11">{location}</div>
                  ) : null}
                </Link>
              );
            })}
          </div>
        ) : null}
      </div>
    </Layout>
  );
}
