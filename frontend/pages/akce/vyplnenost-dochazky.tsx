import React from 'react';
import { useQuery } from 'urql';
import { Layout } from '@/components/layout/Layout';
import { TitleBar } from '@/ui/TitleBar';
import { TrainerAttendanceReportDocument } from '@/graphql/TrainerAttendanceReport';
import { truthyFilter } from '@/ui/truthyFilter';

const numberFormatter = new Intl.NumberFormat('cs-CZ');
const percentFormatter = new Intl.NumberFormat('cs-CZ', {
  style: 'percent',
  maximumFractionDigits: 0,
});

export default function TrainerAttendanceReportPage() {
  const [{ data, fetching, error }] = useQuery({
    query: TrainerAttendanceReportDocument,
    variables: { since: null, until: null },
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

        <section className="space-y-2 text-sm text-muted-foreground">
          <p>
            Přehled vychází z proběhlých termínů vedených.
          </p>
        </section>

        <section className="rounded-lg border border-border bg-neutral-1 p-4 text-sm shadow-sm">
          <p className="font-medium text-foreground">Shrnutí</p>
          {summary.total > 0 ? (
            <ul className="mt-2 list-disc space-y-1 pl-5">
              <li>
                Celkem vedenýhc:{' '}
                <span className="font-semibold text-foreground">{numberFormatter.format(summary.total)}</span>
              </li>
              <li>
                Plně vyplněná docházka:{' '}
                <span className="font-semibold text-foreground">{numberFormatter.format(summary.filled)}</span>
              </li>
              <li>
                Částečně vyplněná docházka:{' '}
                <span className="font-semibold text-foreground">{numberFormatter.format(summary.partial)}</span>
              </li>
              <li>
                Nevyplněná docházka:{' '}
                <span className="font-semibold text-destructive">{numberFormatter.format(summary.unfilled)}</span>
                {summary.ratio !== null ? (
                  <span className="text-muted-foreground">
                    {` · ${percentFormatter.format(summary.ratio)} alespoň částečně vyplněno`}
                  </span>
                ) : null}
              </li>
            </ul>
          ) : (
            <p className="mt-2 text-muted-foreground">Z proběhlých vedených zatím nemáme žádná data.</p>
          )}
        </section>

        {error ? (
          <p className="rounded-md border border-destructive/40 bg-destructive/10 p-3 text-sm text-destructive">
            Nepodařilo se načíst přehled. Zkuste stránku prosím načíst znovu.
          </p>
        ) : null}

        {fetching ? <p className="text-sm text-muted-foreground">Načítáme přehled…</p> : null}

        {!fetching && !error ? (
          rows.length > 0 ? (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-border text-sm">
                <thead>
                  <tr className="bg-neutral-3 text-left text-xs uppercase tracking-wide text-muted-foreground">
                    <th className="px-3 py-2 font-medium">Trenér</th>
                    <th className="px-3 py-2 font-medium text-right">Vedených celkem</th>
                    <th className="px-3 py-2 font-medium text-right">Plně vyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Částečně vyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Nevyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Docházka alespoň částečně</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-border">
                  {rows.map((row) => {
                    const needsAttention = row.pending > 0 || row.unfilled > 0;
                    return (
                      <tr key={row.name} className={needsAttention ? 'bg-amber-50' : undefined}>
                        <td className="px-3 py-2 font-medium text-foreground">{row.name}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.total)}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.filled)}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.partial)}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.unfilled)}</td>
                        <td className="px-3 py-2 text-right text-foreground">
                          {row.ratio !== null ? percentFormatter.format(row.ratio) : '–'}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          ) : (
            <p className="text-sm text-muted-foreground">Momentálně není k dispozici žádný záznam.</p>
          )
        ) : null}
      </div>
    </Layout>
  );
}
