'use client';

import React from 'react';
import { useQuery } from 'urql';
import { PageHeader } from '@/ui/PageHeader';
import { TrainerAttendanceReportDocument } from '@/graphql/TrainerAttendanceReport';
import { isTruthy } from '@/lib/truthyFilter';
import { compareText } from '@/lib/compare';
import { fullDateFormatter } from '@/ui/format';
import { computeRange } from '@/ui/PeriodSelector';
import { Table } from '@/ui/Table';

const numberFormatter = new Intl.NumberFormat('cs-CZ');
const percentFormatter = new Intl.NumberFormat('cs-CZ', {
  style: 'percent',
  maximumFractionDigits: 0,
});

export function AttendanceReport() {
  const now = React.useMemo(() => new Date(), []);
  const period = React.useMemo(() => {
    const period = computeRange('schoolyear', now, null, null);
    if (period.until)
      period.until =
        new Date(period.until).getTime() > now.getTime()
          ? now.toISOString()
          : period.until;
    return period;
  }, [now]);

  const [{ data, fetching, error }] = useQuery({
    query: TrainerAttendanceReportDocument,
    variables: {
      since: period.since,
      until: period.until,
    },
  });

  const { rows, summary } = React.useMemo(() => {
    const source = data?.trainerGroupAttendanceCompletionList ?? [];
    const rows = source.filter(isTruthy).map((row) => ({
      name: row.person?.name ?? '-',
      total: row.totalEvents ?? 0,
      filled: row.filledEvents ?? 0,
      partial: row.partiallyFilledEvents ?? 0,
      unfilled: row.unfilledEvents ?? 0,
      pending: row.pendingAttendances ?? 0,
      ratio: row.filledRatio,
    }));

    rows.sort((a, b) => {
      if (b.unfilled !== a.unfilled) return b.unfilled - a.unfilled;
      const ratioDiff = (a.ratio ?? -1) - (b.ratio ?? -1);
      if (ratioDiff !== 0) return ratioDiff;
      return compareText(a.name, b.name);
    });

    const totals = rows.reduce(
      (acc, row) => ({
        total: acc.total + row.total,
        filled: acc.filled + row.filled,
        partial: acc.partial + row.partial,
        unfilled: acc.unfilled + row.unfilled,
        pending: acc.pending + row.pending,
      }),
      { total: 0, filled: 0, partial: 0, unfilled: 0, pending: 0 },
    );

    return {
      rows,
      summary: {
        ...totals,
        ratio: totals.total > 0 ? (totals.filled + totals.partial) / totals.total : 0,
      },
    };
  }, [data]);

  return (
    <div className="container col-feature space-y-6 py-6">
      <PageHeader
        title="Vyplněnost docházky"
        breadcrumbs={[{ label: 'Akce', href: '/akce' }]}
      />

      <section className="text-sm text-neutral-10">
        {period.displaySince &&
          period.displayUntil &&
          fullDateFormatter.formatRange(period.displaySince, period.displayUntil)}
      </section>

      <section className="rounded-lg border border-neutral-6 bg-neutral-1 p-4 text-sm shadow-xs">
        <p className="font-medium text-neutral-12">Shrnutí</p>
        {summary.total > 0 ? (
          <ul className="mt-2 list-disc space-y-1 pl-5">
            <li>
              Celkem vedených:{' '}
              <span className="font-semibold text-neutral-12">
                {numberFormatter.format(summary.total)}
              </span>
            </li>
            <li>
              Plně vyplněno:{' '}
              <span className="font-semibold text-neutral-12">
                {numberFormatter.format(summary.filled)}
              </span>
            </li>
            <li>
              Částečně vyplněno:{' '}
              <span className="font-semibold text-neutral-12">
                {numberFormatter.format(summary.partial)}
              </span>
            </li>
            <li>
              Nevyplněno:{' '}
              <span className="font-semibold text-accent-11">
                {numberFormatter.format(summary.unfilled)}
              </span>
              <span className="text-neutral-10">
                {` · ${percentFormatter.format(summary.ratio)} alespoň částečně vyplněno`}
              </span>
            </li>
          </ul>
        ) : (
          <p className="mt-2 text-neutral-10">
            Z proběhlých vedených zatím nemáme žádná data.
          </p>
        )}
      </section>

      {error && (
        <p className="rounded-md border border-accent-7 bg-accent-3 p-3 text-sm text-accent-11">
          Nepodařilo se načíst přehled. Zkuste stránku prosím načíst znovu.
        </p>
      )}

      {fetching && <p className="text-sm text-neutral-10">Načítáme přehled…</p>}

      {!fetching && !error && (
        <Table
          className="text-neutral-12"
          columns={[
            'Trenér',
            <span key="total" className="block text-right">Vedených celkem</span>,
            <span key="filled" className="block text-right">Plně vyplněno</span>,
            <span key="partial" className="block text-right">Částečně vyplněno</span>,
            <span key="unfilled" className="block text-right">Nevyplněno</span>,
            <span key="ratio" className="block text-right">Docházka alespoň částečně</span>,
          ]}
          empty="Momentálně není k dispozici žádný záznam."
        >
          {rows.map((row) => (
            <tr
              key={row.name}
              className={row.pending > 0 || row.unfilled > 0 ? 'bg-accent-3/30' : undefined}
            >
              <td className="font-medium">{row.name}</td>
              <td className="text-right">{numberFormatter.format(row.total)}</td>
              <td className="text-right">{numberFormatter.format(row.filled)}</td>
              <td className="text-right">{numberFormatter.format(row.partial)}</td>
              <td className="text-right">{numberFormatter.format(row.unfilled)}</td>
              <td className="text-right">
                {row.ratio !== null ? percentFormatter.format(row.ratio) : '–'}
              </td>
            </tr>
          ))}
        </Table>
      )}
    </div>
  );
}
