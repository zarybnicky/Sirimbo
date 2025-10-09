import React from 'react';
import { useQuery, gql } from 'urql';
import type { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { Layout } from '@/components/layout/Layout';
import { TitleBar } from '@/ui/TitleBar';

const numberFormatter = new Intl.NumberFormat('cs-CZ');
const percentFormatter = new Intl.NumberFormat('cs-CZ', {
  style: 'percent',
  maximumFractionDigits: 0,
});
const nameCollator = new Intl.Collator('cs');

const TrainerAttendanceReportDocument = gql`
  query TrainerAttendanceReport($since: Datetime, $until: Datetime) {
    trainerGroupAttendanceCompletionList(since: $since, until: $until) {
      personId
      totalInstances
      filledInstances
      partiallyFilledInstances
      unfilledInstances
      filledRatio
      pendingAttendances
      person {
        id
        name
      }
    }
  }
` satisfies TypedDocumentNode;

type TrainerAttendanceReportQuery = typeof TrainerAttendanceReportDocument extends TypedDocumentNode<
  infer TData,
  any
>
  ? TData
  : never;

type TrainerAttendanceRow = NonNullable<
  NonNullable<TrainerAttendanceReportQuery['trainerGroupAttendanceCompletionList']>[number]
>;

const parseCount = (value?: string | null) => {
  const parsed = Number(value ?? 0);
  return Number.isFinite(parsed) ? parsed : 0;
};

type NormalizedRow = {
  id: string;
  name: string;
  total: number;
  filled: number;
  partial: number;
  unfilled: number;
  pending: number;
  ratio: number | null;
};

export default function TrainerAttendanceReportPage() {
  const [{ data, fetching, error }] = useQuery({
    query: TrainerAttendanceReportDocument,
    variables: { since: null, until: null },
  });

  const { rows, summary } = React.useMemo(() => {
    const source = data?.trainerGroupAttendanceCompletionList ?? [];
    const rows: NormalizedRow[] = source
      .filter((row: TrainerAttendanceRow | null | undefined): row is TrainerAttendanceRow => Boolean(row))
      .map((row: TrainerAttendanceRow): NormalizedRow => ({
        id: row.personId,
        name: row.person?.name ?? '—',
        total: parseCount(row.totalInstances),
        filled: parseCount(row.filledInstances),
        partial: parseCount(row.partiallyFilledInstances),
        unfilled: parseCount(row.unfilledInstances),
        pending: parseCount(row.pendingAttendances),
        ratio: typeof row.filledRatio === 'number' ? row.filledRatio : null,
      }));

    rows.sort((a, b) => {
      if (b.unfilled !== a.unfilled) return b.unfilled - a.unfilled;
      const ratioDiff = (a.ratio ?? -1) - (b.ratio ?? -1);
      if (ratioDiff !== 0) return ratioDiff;
      const nameDiff = nameCollator.compare(a.name, b.name);
      if (nameDiff !== 0) return nameDiff;
      return a.id.localeCompare(b.id, 'cs');
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
            Přehled vychází z minulých instancí skupinových lekcí, které nebyly zrušeny. Hodiny bez žádného záznamu nebo pouze se
            stavem „nezadáno“ se uvádějí jako nevyplněné, zvlášť sledujeme částečně a plně vyplněné případy.
          </p>
          <p>
            Do výpočtu se započítávají trenéři přiřazení přímo k instanci i k celé události. Údaje se vztahují pouze k aktuálně
            zvolenému tenantovi.
          </p>
        </section>

        <section className="rounded-lg border border-border bg-neutral-1 p-4 text-sm shadow-sm">
          <p className="font-medium text-foreground">Shrnutí</p>
          {summary.total > 0 ? (
            <ul className="mt-2 list-disc space-y-1 pl-5">
              <li>
                Celkem sledovaných hodin:{' '}
                <span className="font-semibold text-foreground">{numberFormatter.format(summary.total)}</span>
              </li>
              <li>
                Plně vyplněné hodiny:{' '}
                <span className="font-semibold text-foreground">{numberFormatter.format(summary.filled)}</span>
              </li>
              <li>
                Částečně vyplněné hodiny:{' '}
                <span className="font-semibold text-foreground">{numberFormatter.format(summary.partial)}</span>
              </li>
              <li>
                Hodiny s nevyplněnou docházkou:{' '}
                <span className="font-semibold text-destructive">{numberFormatter.format(summary.unfilled)}</span>
                {summary.ratio !== null ? (
                  <span className="text-muted-foreground">
                    {` · ${percentFormatter.format(summary.ratio)} alespoň částečně vyplněno`}
                  </span>
                ) : null}
              </li>
              <li>
                Zbývající jednotlivé záznamy docházky:{' '}
                <span className="font-semibold text-destructive">{numberFormatter.format(summary.pending)}</span>
              </li>
            </ul>
          ) : (
            <p className="mt-2 text-muted-foreground">Z minulých skupinových lekcí zatím nemáme žádná data.</p>
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
                    <th className="px-3 py-2 font-medium text-right">Hodin celkem</th>
                    <th className="px-3 py-2 font-medium text-right">Plně vyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Částečně vyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Nevyplněno</th>
                    <th className="px-3 py-2 font-medium text-right">Docházka alespoň částečně</th>
                    <th className="px-3 py-2 font-medium text-right">Nezadané záznamy</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-border">
                  {rows.map((row) => {
                    const needsAttention = row.pending > 0 || row.unfilled > 0;
                    return (
                      <tr key={row.id} className={needsAttention ? 'bg-amber-50' : undefined}>
                        <td className="px-3 py-2 font-medium text-foreground">{row.name}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.total)}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.filled)}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.partial)}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.unfilled)}</td>
                        <td className="px-3 py-2 text-right text-foreground">
                          {row.ratio !== null ? percentFormatter.format(row.ratio) : '–'}
                        </td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(row.pending)}</td>
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
