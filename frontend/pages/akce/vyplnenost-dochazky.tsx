import React from 'react';
import { useQuery } from 'urql';
import { Layout } from '@/components/layout/Layout';
import { TitleBar } from '@/ui/TitleBar';
import { TrainerAttendanceReportDocument } from '@/graphql/TrainerAttendanceReport';
import { truthyFilter } from '@/ui/truthyFilter';
import { computeRange } from '@/scoreboard/periods';
import { fullDateFormatter } from '@/ui/format';

const numberFormatter = new Intl.NumberFormat('cs-CZ');
const percentFormatter = new Intl.NumberFormat('cs-CZ', {
  style: 'percent',
  maximumFractionDigits: 0,
});

export default function TrainerAttendanceReportPage() {
  const period = React.useMemo(() => {
    const period = computeRange('schoolYear', new Date(), null, null);
    if (period.until)
      period.until = (new Date(period.until).getTime() > Date.now()) ? new Date().toISOString() : period.until;
    return period;
  }, []);

  const [{ data, fetching, error }] = useQuery({
    query: TrainerAttendanceReportDocument,
    variables: {
      since: period.since,
      until: period.until,
    },
  });

  const { rows, summary } = React.useMemo(() => {
    const source = data?.trainerGroupAttendanceCompletionList ?? [];
    const rows = source
      .filter(truthyFilter)
      .map((row) => ({
        name: row.person?.name ?? '—',
        total: row.totalInstances ?? 0,
        filled: row.filledInstances ?? 0,
        partial: row.partiallyFilledInstances ?? 0,
        unfilled: row.unfilledInstances ?? 0,
        pending: row.pendingAttendances ?? 0,
        ratio: row.filledRatio,
      }));

    rows.sort((a, b) => {
      if (b.unfilled !== a.unfilled) return b.unfilled - a.unfilled;
      const ratioDiff = (a.ratio ?? -1) - (b.ratio ?? -1);
      if (ratioDiff !== 0) return ratioDiff;
      return a.name.localeCompare(b.name);
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
        ratio: totals.total > 0 ? (totals.filled + totals.partial) / totals.total : null,
      },
    };
  }, [data]);

  return (
    <Layout requireAdmin>
      <div className="container col-feature space-y-6 py-6">
        <TitleBar title="Docházka trenérů – skupinové lekce" />

        <section className="space-y-2 text-sm text-neutral-10">
          <p>
            Přehled vychází z proběhlých termínů vedených.
          </p>
          {period.displaySince && period.displayUntil ? (
            <p>
              {fullDateFormatter.formatRange(period.displaySince, period.displayUntil)}
            </p>
          ) : null}
        </section>

        <section className="rounded-lg border border-neutral-6 bg-neutral-1 p-4 text-sm shadow-sm">
          <p className="font-medium text-neutral-12">Shrnutí</p>
          {summary.total > 0 ? (
            <ul className="mt-2 list-disc space-y-1 pl-5">
              <li>
                Celkem vedených:{' '}
                <span className="font-semibold text-neutral-12">{numberFormatter.format(summary.total)}</span>
              </li>
              <li>
                Plně vyplněná docházka:{' '}
                <span className="font-semibold text-neutral-12">{numberFormatter.format(summary.filled)}</span>
              </li>
              <li>
                Částečně vyplněná docházka:{' '}
                <span className="font-semibold text-neutral-12">{numberFormatter.format(summary.partial)}</span>
              </li>
              <li>
                Nevyplněná docházka:{' '}
                <span className="font-semibold text-accent-11">{numberFormatter.format(summary.unfilled)}</span>
                {summary.ratio !== null ? (
                  <span className="text-neutral-10">
                    {` · ${percentFormatter.format(summary.ratio)} alespoň částečně vyplněno`}
                  </span>
                ) : null}
              </li>
            </ul>
          ) : (
            <p className="mt-2 text-neutral-10">Z proběhlých vedených zatím nemáme žádná data.</p>
          )}
        </section>

        {error ? (
          <p className="rounded-md border border-accent-7 bg-accent-3 p-3 text-sm text-accent-11">
            Nepodařilo se načíst přehled. Zkuste stránku prosím načíst znovu.
          </p>
        ) : null}

        {fetching ? <p className="text-sm text-neutral-10">Načítáme přehled…</p> : null}

        {!fetching && !error ? (
          rows.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-neutral-6 text-sm">
                <thead>
                  <tr className="bg-neutral-3 text-left text-xs uppercase tracking-wide text-neutral-10">
                    <th className="px-3 py-2 font-medium">Trenér</th>
                    <th className="px-3 py-2 font-medium text-right">Vedených celkem</th>
                    <th className="px-3 py-2 font-medium text-right">Plně vyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Částečně vyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Nevyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Docházka alespoň částečně</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-neutral-6">
                  {rows.map((row) => {
                    const needsAttention = row.pending > 0 || row.unfilled > 0;
                    return (
                      <tr key={row.name} className={needsAttention ? 'bg-accent-3/30' : undefined}>
                        <td className="px-3 py-2 font-medium text-neutral-12">{row.name}</td>
                        <td className="px-3 py-2 text-right text-neutral-12">{numberFormatter.format(row.total)}</td>
                        <td className="px-3 py-2 text-right text-neutral-12">{numberFormatter.format(row.filled)}</td>
                        <td className="px-3 py-2 text-right text-neutral-12">{numberFormatter.format(row.partial)}</td>
                        <td className="px-3 py-2 text-right text-neutral-12">{numberFormatter.format(row.unfilled)}</td>
                        <td className="px-3 py-2 text-right text-neutral-12">
                          {row.ratio !== null ? percentFormatter.format(row.ratio) : '–'}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          ) : (
            <p className="text-sm text-neutral-10">Momentálně není k dispozici žádný záznam.</p>
          )
        ) : null}
      </div>
    </Layout>
  );
}
