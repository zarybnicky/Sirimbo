import type { EventType } from '@/graphql';
import {
  EventRangeDocument,
  type EventRangeQuery,
} from '@/graphql/Event';
import {
  capitalize,
  dateTimeFormatter,
  formatDuration,
  formatEventName,
  monthFormatter,
} from '@/ui/format';
import Link from 'next/link';
import { parseAsString, useQueryStates } from 'nuqs';
import React from 'react';
import { useQuery } from 'urql';

type ReportRow = {
  event: NonNullable<EventRangeQuery['list']>[number];
  title: string;
  location: string;
  durationMinutes: number;
  attended: number;
  total: number;
};

const reportTypes: Record<'groupRows' | 'privateRows', EventType> = {
  groupRows: 'GROUP',
  privateRows: 'LESSON',
};

const toMonthKey = (date: Date) =>
  `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}`;

export const PersonWorkReportView = React.memo(function PersonWorkReportView({ id }: { id: string }) {
  const now = React.useMemo(() => new Date(), []);
  const month = React.useMemo(() => new Date(now.getFullYear(), now.getMonth(), 1), [now]);
  const workReportQueryParsers = React.useMemo(
    () => ({
      tab: parseAsString,
      month: parseAsString.withDefault(
        `${month.getFullYear()}-${String(month.getMonth() + 1).padStart(2, '0')}`,
      ),
    }),
    [month],
  );
  const [{ month: monthKey }, setWorkReportQuery] = useQueryStates(
    workReportQueryParsers,
    { history: 'push' },
  );

  const months = React.useMemo(() => {
    return Array.from({ length: 8 }, (_, i) => {
      const since = new Date(month.getFullYear(), month.getMonth() - i, 1);
      return {
        key: toMonthKey(since),
        since,
        label: capitalize(monthFormatter.format(since)),
      };
    });
  }, [month]);

  const [{ data, fetching, error }] = useQuery({
    query: EventRangeDocument,
    variables: {
      start: months.at(-1)?.since.toISOString() ?? month.toISOString(),
      end: now.toISOString(),
      trainerIds: [id],
    },
    pause: !id,
  });

  const reports = React.useMemo(() => {
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

    for (const event of data?.list ?? []) {
      if (event.isCancelled) continue;
      if (new Date(event.until).getTime() > now.getTime()) continue;
      if (!event.trainersList?.some((trainer) => trainer.personId === id)) continue;

      if (event.type !== 'GROUP' && event.type !== 'LESSON') continue;

      const since = new Date(event.since);
      const until = new Date(event.until);
      const month = byKey.get(toMonthKey(since));
      if (!month) continue;

      const stats =
        typeof event.stats === 'string' ? JSON.parse(event.stats) : event.stats;
      const row: ReportRow = {
        event: event,
        title: formatEventName(event) || '-',
        location: event.location?.name || event.locationText || '—',
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

      month[event.type === 'GROUP' ? 'groupRows' : 'privateRows'].push(row);
    }

    for (const month of byKey.values()) {
      month.groupRows.sort((a, b) => a.event.since.localeCompare(b.event.since));
      month.privateRows.sort((a, b) => a.event.since.localeCompare(b.event.since));
    }

    return [...byKey.values()];
  }, [data?.list, id, months, now]);

  const selectedMonth = reports.find((month) => month.key === monthKey) ?? reports[0];

  return (
    <div className="grid gap-6">
      {error && (
        <p className="rounded-md border border-accent-7 bg-accent-3 p-3 text-sm text-accent-11">
          Nepodařilo se načíst výkaz práce. Zkuste stránku prosím načíst znovu.
        </p>
      )}

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
            {reports.map((month) => {
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
                    <button
                      type="button"
                      className="text-left text-accent-11 hover:underline"
                      onClick={() => {
                        void setWorkReportQuery({
                          tab: 'workReport',
                          month: month.key,
                        });
                      }}
                    >
                      {month.label}
                    </button>
                  </td>
                  <td className="whitespace-nowrap text-right tabular-nums">
                    {month.groupRows.length} /{' '}
                    {formatDuration(groupMinutes)}
                  </td>
                  <td className="whitespace-nowrap text-right tabular-nums">
                    {month.privateRows.length} /{' '}
                    {formatDuration(privateMinutes)}
                  </td>
                  <td className="whitespace-nowrap text-right tabular-nums">
                    {rowCount} /{' '}
                    {formatDuration(groupMinutes + privateMinutes)}
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </section>

      {fetching && !data && (
        <p className="text-sm text-neutral-10">Načítáme výkaz…</p>
      )}

      {selectedMonth && (
        <section className="prose prose-accent max-w-none">
          <h3>{selectedMonth.label}</h3>

          {!selectedMonth.groupRows?.length && !selectedMonth?.privateRows?.length && !fetching && !error && (
            <p>V tomto měsíci nic neproběhlo</p>
          )}

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
                      {reportTypes[rowsKey] === 'GROUP' && (
                        <th className="text-right">Účast</th>
                      )}
                      <th className="text-right">Délka</th>
                    </tr>
                  </thead>
                  <tbody>
                    {rows.map((row) => (
                      <tr key={row.event.id}>
                        <td className="whitespace-nowrap">
                          {dateTimeFormatter.format(new Date(row.event.since))}
                        </td>
                        <td>
                          <Link href={`/termin/${row.event.id}`}>
                            {row.title}
                          </Link>
                        </td>
                        <td>{row.location}</td>
                        {reportTypes[rowsKey] === 'GROUP' && (
                          <td className="whitespace-nowrap text-right tabular-nums">
                            {row.attended} / {row.total}
                          </td>
                        )}
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
      )}
    </div>
  );
});
