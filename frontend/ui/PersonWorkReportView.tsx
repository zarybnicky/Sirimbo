import type { EventType } from '@/graphql';
import {
  EventInstanceRangeDocument,
  type EventInstanceRangeQuery,
} from '@/graphql/Event';
import {
  dateTimeFormatter,
  formatDefaultEventName,
  formatEventType,
} from '@/ui/format';
import Link from 'next/link';
import { useRouter } from 'next/router';
import React from 'react';
import { StringParam, useQueryParam, withDefault } from 'use-query-params';
import { useQuery } from 'urql';

type EventInstance = NonNullable<EventInstanceRangeQuery['list']>[number];
type EventInstanceWithEvent = EventInstance & {
  event: NonNullable<EventInstance['event']>;
};

type ReportRow = {
  instance: EventInstanceWithEvent;
  title: string;
  location: string;
  durationMinutes: number;
  attended: number;
  total: number;
};

type MonthReport = {
  key: string;
  label: string;
  since: Date;
  groupRows: ReportRow[];
  privateRows: ReportRow[];
};

const numberFormatter = new Intl.NumberFormat('cs-CZ');
const hoursFormatter = new Intl.NumberFormat('cs-CZ', {
  maximumFractionDigits: 1,
  minimumFractionDigits: 0,
});
const monthFormatter = new Intl.DateTimeFormat('cs-CZ', {
  month: 'long',
  year: 'numeric',
});

const reportTypes: Record<'groupRows' | 'privateRows', EventType> = {
  groupRows: 'GROUP',
  privateRows: 'LESSON',
};

function formatDuration(minutes: number) {
  if (minutes < 60) return `${numberFormatter.format(minutes)} min`;
  return `${hoursFormatter.format(minutes / 60)} h`;
}

const toMonthKey = (date: Date) => `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}`;

export function PersonWorkReportView({ id }: { id: string }) {
  const router = useRouter();
  const reportNow = React.useMemo(() => new Date(), []);
  const currentMonth = React.useMemo(
    () => new Date(reportNow.getFullYear(), reportNow.getMonth(), 1),
    [reportNow],
  );
  const currentMonthKey = `${currentMonth.getFullYear()}-${String(
    currentMonth.getMonth() + 1,
  ).padStart(2, '0')}`;
  const [selectedMonthKey] = useQueryParam(
    'workReportMonth',
    withDefault(StringParam, currentMonthKey),
  );

  const months = React.useMemo<MonthReport[]>(() => {
    return Array.from({ length: 8 }, (_, index) => {
      const since = new Date(
        currentMonth.getFullYear(),
        currentMonth.getMonth() - index,
        1,
      );
      const label = monthFormatter.format(since);
      return {
        key: toMonthKey(since),
        label: label.slice(0, 1).toUpperCase() + label.slice(1),
        since,
        groupRows: [],
        privateRows: [],
      };
    });
  }, [currentMonth]);

  const [{ data, fetching, error }] = useQuery({
    query: EventInstanceRangeDocument,
    variables: {
      start: months.at(-1)?.since.toISOString() ?? currentMonth.toISOString(),
      end: reportNow.toISOString(),
      trainerIds: [id],
    },
    pause: !id,
    requestPolicy: 'cache-and-network',
  });

  const monthReports = React.useMemo(() => {
    const byKey = new Map(
      months.map((month) => [
        month.key,
        {
          ...month,
          groupRows: [] as ReportRow[],
          privateRows: [] as ReportRow[],
        },
      ]),
    );
    const reportNowTime = reportNow.getTime();

    for (const instance of data?.list ?? []) {
      if (!instance.event || instance.isCancelled) continue;
      if (new Date(instance.until).getTime() > reportNowTime) continue;
      if (!instance.trainersList?.some((trainer) => trainer.personId === id)) continue;

      const type = instance.type ?? instance.event.type;
      if (type !== 'GROUP' && type !== 'LESSON') continue;

      const since = new Date(instance.since);
      const until = new Date(instance.until);
      const month = byKey.get(toMonthKey(since));
      if (!month) continue;

      const stats =
        typeof instance.stats === 'string' ? JSON.parse(instance.stats) : instance.stats;
      const row: ReportRow = {
        instance: { ...instance, event: instance.event },
        title: instance.name || formatDefaultEventName(instance.event),
        location: instance.location?.name || instance.locationText || '—',
        durationMinutes: Math.max(
          0,
          Math.round((until.getTime() - since.getTime()) / 60_000),
        ),
        attended:
          stats && typeof stats === 'object' && typeof stats.ATTENDED === 'number'
            ? stats.ATTENDED
            : 0,
        total:
          stats && typeof stats === 'object' && typeof stats.TOTAL === 'number'
            ? stats.TOTAL
            : 0,
      };

      month[type === 'GROUP' ? 'groupRows' : 'privateRows'].push(row);
    }

    for (const month of byKey.values()) {
      month.groupRows.sort((a, b) => a.instance.since.localeCompare(b.instance.since));
      month.privateRows.sort((a, b) =>
        a.instance.since.localeCompare(b.instance.since),
      );
    }

    return [...byKey.values()];
  }, [data?.list, id, months, reportNow]);

  const selectedMonth =
    monthReports.find((month) => month.key === selectedMonthKey) ?? monthReports[0];
  const selectedRows = [
    ...(selectedMonth?.groupRows ?? []),
    ...(selectedMonth?.privateRows ?? []),
  ];

  return (
    <div className="grid gap-6">
      {error ? (
        <p className="rounded-md border border-accent-7 bg-accent-3 p-3 text-sm text-accent-11">
          Nepodařilo se načíst výkaz práce. Zkuste stránku prosím načíst znovu.
        </p>
      ) : null}

      <section className="prose prose-accent max-w-none">
        <h3>Měsíce</h3>
          <table>
            <thead>
              <tr>
                <th>Měsíc</th>
                <th className="text-right">Vedené</th>
                <th className="text-right">Individuálky</th>
                <th className="text-right">Celkem</th>
              </tr>
            </thead>
            <tbody>
              {monthReports.map((month) => {
                const groupMinutes = month.groupRows.reduce(
                  (total, row) => total + row.durationMinutes,
                  0,
                );
                const privateMinutes = month.privateRows.reduce(
                  (total, row) => total + row.durationMinutes,
                  0,
                );
                const rowCount = month.groupRows.length + month.privateRows.length;

                return (
                  <tr
                    key={month.key}
                    className={month.key === selectedMonth?.key ? 'bg-accent-3/50' : ''}
                  >
                    <td>
                      <Link
                        href={{
                          pathname: router.pathname,
                          query: {
                            ...router.query,
                            tab: 'workReport',
                            workReportMonth: month.key,
                          },
                        }}
                      >
                        {month.label}
                      </Link>
                    </td>
                    <td className="whitespace-nowrap text-right tabular-nums">
                      {numberFormatter.format(month.groupRows.length)} /{' '}
                      {formatDuration(groupMinutes)}
                    </td>
                    <td className="whitespace-nowrap text-right tabular-nums">
                      {numberFormatter.format(month.privateRows.length)} /{' '}
                      {formatDuration(privateMinutes)}
                    </td>
                    <td className="whitespace-nowrap text-right tabular-nums">
                      {numberFormatter.format(rowCount)} /{' '}
                      {formatDuration(groupMinutes + privateMinutes)}
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
      </section>

      {fetching && !data ? (
        <p className="text-sm text-neutral-10">Načítáme výkaz…</p>
      ) : null}

      {selectedMonth ? (
        <section className="prose prose-accent max-w-none">
          <h3>{selectedMonth.label}</h3>

          {selectedRows.length === 0 && !fetching && !error ? (
            <p>
              V tomto měsíci nic neproběhlo
            </p>
          ) : null}

          {(
            [
              ['Vedené', 'groupRows'],
              ['Individuálky', 'privateRows'],
            ] as const
          ).map(([title, rowsKey]) => {
            const rows = selectedMonth[rowsKey];
            if (rows.length === 0) return null;

            return (
              <React.Fragment key={rowsKey}>
                <h4>{title}</h4>
                  <table>
                    <thead>
                      <tr>
                        <th>Termín</th>
                        <th>Událost</th>
                        <th>Místo</th>
                        {reportTypes[rowsKey] === 'GROUP' ? (
                          <th className="text-right">Účast</th>
                        ) : null}
                        <th className="text-right">Délka</th>
                      </tr>
                    </thead>
                    <tbody>
                      {rows.map((row) => (
                        <tr key={row.instance.id}>
                          <td className="whitespace-nowrap">
                            {dateTimeFormatter.format(new Date(row.instance.since))}
                          </td>
                          <td>
                            <Link
                              href={{
                                pathname: '/akce/[id]/termin/[instance]',
                                query: {
                                  id: row.instance.event.id,
                                  instance: row.instance.id,
                                },
                              }}
                            >
                              {row.title}
                            </Link>
                          </td>
                          <td>{row.location}</td>
                          {reportTypes[rowsKey] === 'GROUP' ? (
                            <td className="whitespace-nowrap text-right tabular-nums">
                              {numberFormatter.format(row.attended)} /{' '}
                              {numberFormatter.format(row.total)}
                            </td>
                          ) : null}
                          <td className="whitespace-nowrap text-right tabular-nums">
                            {formatDuration(row.durationMinutes)}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
              </React.Fragment>
            );
          })}
        </section>
      ) : null}
    </div>
  );
}
