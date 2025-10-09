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

type TrainerAttendanceRow = {
  __typename?: 'TrainerGroupAttendanceCompletion';
  trainerId: string;
  trainerName: string;
  totalInstances: string;
  filledInstances: string;
  partiallyFilledInstances: string;
  unfilledInstances: string;
  filledRatio: number | null;
  totalAttendances: string;
  pendingAttendances: string;
};

type TrainerAttendanceReportQuery = {
  __typename?: 'Query';
  trainerGroupAttendanceCompletionList: TrainerAttendanceRow[] | null;
};

type TrainerAttendanceReportQueryVariables = Record<string, never>;

const TrainerAttendanceReportDocument: TypedDocumentNode<
  TrainerAttendanceReportQuery,
  TrainerAttendanceReportQueryVariables
> = gql`
  query TrainerAttendanceReport {
    trainerGroupAttendanceCompletionList {
      trainerId
    trainerName
    totalInstances
    filledInstances
    partiallyFilledInstances
    unfilledInstances
    filledRatio
      totalAttendances
      pendingAttendances
    }
  }
`;

function parseCount(value: string | null | undefined) {
  if (typeof value !== 'string') {
    return 0;
  }

  const parsed = Number(value);
  return Number.isFinite(parsed) ? parsed : 0;
}

export default function TrainerAttendanceReportPage() {
  const [{ data, fetching, error }] = useQuery({ query: TrainerAttendanceReportDocument });

  const rows = React.useMemo(() => {
    const list = data?.trainerGroupAttendanceCompletionList ?? [];

    return [...list].sort((a, b) => {
      const unfilledDiff = parseCount(b.unfilledInstances) - parseCount(a.unfilledInstances);
      if (unfilledDiff !== 0) {
        return unfilledDiff;
      }

      const ratioA = typeof a.filledRatio === 'number' ? a.filledRatio : -1;
      const ratioB = typeof b.filledRatio === 'number' ? b.filledRatio : -1;
      if (ratioA !== ratioB) {
        return ratioA - ratioB;
      }

      return a.trainerName.localeCompare(b.trainerName, 'cs');
    });
  }, [data?.trainerGroupAttendanceCompletionList]);

  const summary = React.useMemo(() => {
    const totalInstances = rows.reduce((sum, row) => sum + parseCount(row.totalInstances), 0);
    const fullyFilledInstances = rows.reduce((sum, row) => sum + parseCount(row.filledInstances), 0);
    const partiallyFilledInstances = rows.reduce(
      (sum, row) => sum + parseCount(row.partiallyFilledInstances),
      0,
    );
    const unfilledInstances = rows.reduce((sum, row) => sum + parseCount(row.unfilledInstances), 0);
    const pendingAttendances = rows.reduce((sum, row) => sum + parseCount(row.pendingAttendances), 0);
    const filledRatio =
      totalInstances > 0 ? (fullyFilledInstances + partiallyFilledInstances) / totalInstances : null;

    return {
      totalInstances,
      fullyFilledInstances,
      partiallyFilledInstances,
      unfilledInstances,
      pendingAttendances,
      filledRatio,
    };
  }, [rows]);

  return (
    <Layout requireAdmin>
      <div className="container col-feature space-y-6 py-6">
        <TitleBar title="Docházka trenérů – skupinové lekce" />

        <section className="space-y-2 text-sm text-muted-foreground">
          <p>
            Přehled vychází z minulých instancí skupinových lekcí, které nebyly zrušeny. Hodiny bez žádného záznamu
            nebo pouze se stavem „nezadáno“ se uvádějí jako nevyplněné, zvlášť sledujeme částečně a plně vyplněné
            případy.
          </p>
          <p>
            Do výpočtu se započítávají trenéři přiřazení přímo k instanci i k celé události. Údaje se vztahují pouze k
            aktuálně zvolenému tenantovi.
          </p>
        </section>

        <section className="rounded-lg border border-border bg-neutral-1 p-4 text-sm shadow-sm">
          <p className="font-medium text-foreground">Shrnutí</p>
          {summary.totalInstances > 0 ? (
            <ul className="mt-2 list-disc space-y-1 pl-5">
              <li>
                Celkem sledovaných hodin:{' '}
                <span className="font-semibold text-foreground">{numberFormatter.format(summary.totalInstances)}</span>
              </li>
              <li>
                Plně vyplněné hodiny:{' '}
                <span className="font-semibold text-foreground">
                  {numberFormatter.format(summary.fullyFilledInstances)}
                </span>
              </li>
              <li>
                Částečně vyplněné hodiny:{' '}
                <span className="font-semibold text-foreground">
                  {numberFormatter.format(summary.partiallyFilledInstances)}
                </span>
              </li>
              <li>
                Hodiny s nevyplněnou docházkou:{' '}
                <span className="font-semibold text-destructive">
                  {numberFormatter.format(summary.unfilledInstances)}
                </span>
                {summary.totalInstances > 0 && typeof summary.filledRatio === 'number' ? (
                  <span className="text-muted-foreground">
                    {` · ${percentFormatter.format(summary.filledRatio)} alespoň částečně vyplněno`}
                  </span>
                ) : null}
              </li>
              <li>
                Zbývající jednotlivé záznamy docházky:{' '}
                <span className="font-semibold text-destructive">
                  {numberFormatter.format(summary.pendingAttendances)}
                </span>
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
                    const total = parseCount(row.totalInstances);
                    const filled = parseCount(row.filledInstances);
                    const partial = parseCount(row.partiallyFilledInstances);
                    const unfilled = parseCount(row.unfilledInstances);
                    const pending = parseCount(row.pendingAttendances);
                    const ratio = typeof row.filledRatio === 'number' ? row.filledRatio : null;

                    const needsAttention = pending > 0 || unfilled > 0;

                    return (
                      <tr key={row.trainerId} className={needsAttention ? 'bg-amber-50' : undefined}>
                        <td className="px-3 py-2 font-medium text-foreground">{row.trainerName}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(total)}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(filled)}</td>
                        <td className="px-3 py-2 text-right text-foreground">{numberFormatter.format(partial)}</td>
                        <td className="px-3 py-2 text-right text-foreground">
                          {numberFormatter.format(unfilled)}
                        </td>
                        <td className="px-3 py-2 text-right text-foreground">
                          {ratio !== null ? percentFormatter.format(ratio) : '–'}
                        </td>
                        <td className="px-3 py-2 text-right text-foreground">
                          {numberFormatter.format(pending)}
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
