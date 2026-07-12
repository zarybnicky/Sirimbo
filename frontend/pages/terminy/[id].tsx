import type { AttendanceType } from '@/graphql';
import { EventSeriesDocument, type EventInstanceFragment } from '@/graphql/Event';
import { cn } from '@/lib/cn';
import { Layout } from '@/ui/Layout';
import { Spinner } from '@/ui/Spinner';
import { TabMenu } from '@/ui/TabMenu';
import { PageHeader } from '@/ui/TitleBar';
import { FormError } from '@/ui/form';
import { formatEventType, fullDateFormatter } from '@/ui/format';
import { useAuth } from '@/ui/use-auth';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { Check, HelpCircle, type LucideIcon, X } from 'lucide-react';
import { NextSeo } from 'next-seo';
import Link from 'next/link';
import { parseAsString, useQueryState } from 'nuqs';
import * as React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

const labels: { [key in AttendanceType]: LucideIcon } = {
  ATTENDED: Check,
  UNKNOWN: HelpCircle,
  NOT_EXCUSED: X,
};

export default function EventSeriesPage() {
  const {
    query: { id },
  } = useTypedRouter(QueryParams);
  const auth = useAuth();
  const [{ data, error, fetching }] = useQuery({
    query: EventSeriesDocument,
    variables: { id },
    pause: !id,
  });
  const series = data?.eventSeries;
  const title = series?.name || 'Termíny';
  const [variant, setVariant] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );
  const tabs = React.useMemo(() => {
    if (!series) return [];
    const tabs: {
      id: string;
      title: React.ReactNode;
      contents: () => React.ReactNode;
    }[] = [];
    if (auth.isTrainerOrAdmin) {
      tabs.push({
        id: 'attendance',
        title: 'Účast',
        contents: () => <Attendance instances={series.attendanceEventsList} />,
      });
    }
    tabs.push({
      id: 'instances',
      title: 'Termíny',
      contents: () => <EventInstances instances={series.eventsList} />,
    });
    return tabs;
  }, [auth.isTrainerOrAdmin, series]);

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={title} />
      <div className="col-feature min-h-[60vh] p-4 lg:pb-8">
        <PageHeader title={title} />
        <FormError error={error} />
        {fetching && !series ? <Spinner /> : null}
        {!fetching && !series ? <p>Série termínů nebyla nalezena.</p> : null}
        {series?.eventsList.length === 0 ? <p>Série nemá žádné termíny.</p> : null}
        {series?.eventsList.length && (
          <div className="max-w-full">
            <TabMenu selected={variant} onSelect={setVariant} options={tabs} />
          </div>
        )}
      </div>
    </Layout>
  );
}

function Attendance({ instances }: { instances: EventInstanceFragment[] }) {
  return (
    <table className="prose prose-accent max-w-none">
      <thead>
        <tr>
          <th />
          {Object.entries(labels).map(([key, Icon]) => (
            <th className="text-center" key={key}>
              {React.createElement(Icon, { className: 'inline-block' })}
            </th>
          ))}
        </tr>
      </thead>
      <tbody>
        {instances.map((instance) => {
          const stats =
            typeof instance.stats === 'string' ? JSON.parse(instance.stats) : instance.stats;

          return (
            <tr key={instance.id}>
              <td>
                <Link href={`/termin/${instance.id}`}>
                  {fullDateFormatter.formatRange(
                    new Date(instance.since),
                    new Date(instance.until),
                  )}
                </Link>
              </td>
              {Object.keys(labels).map((status) => (
                <td className="text-center" key={status}>
                  {stats?.[status] ?? 0}
                </td>
              ))}
            </tr>
          );
        })}
      </tbody>
    </table>
  );
}

function EventInstances({ instances }: { instances: EventInstanceFragment[] }) {
  return (
    <div className="divide-y divide-neutral-4 rounded-lg border border-neutral-4 bg-neutral-1">
      {instances.map((instance) => {
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
            {location ? <div className="text-sm text-neutral-11">{location}</div> : null}
          </Link>
        );
      })}
    </div>
  );
}
