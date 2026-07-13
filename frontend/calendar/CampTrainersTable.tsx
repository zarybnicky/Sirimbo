import {
  CampTrainerOverviewDocument,
  type CampTrainerOverviewQuery,
} from '@/graphql/Event';
import { FormError } from '@/ui/form';
import { moneyFormatter } from '@/ui/format';
import { Spinner } from '@/ui/Spinner';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { type Column, DataGrid } from 'react-data-grid';
import { useQuery } from 'urql';

type ScheduledEvent = NonNullable<CampTrainerOverviewQuery['scheduledEvents']>[number];
type Row = {
  id: string;
  trainer: string;
  eventsByDay: Map<string, ScheduledEvent[]>;
  payouts: Map<string, number>;
  payoutIncomplete: boolean;
};

const dayKeyFormatter = new Intl.DateTimeFormat('sv-SE');
const dayFormatter = new Intl.DateTimeFormat('cs-CZ', {
  weekday: 'short',
  day: 'numeric',
  month: 'numeric',
});

export function CampTrainersTable({
  id,
  since,
  until,
}: {
  id: string;
  since: string;
  until: string;
}) {
  const [query] = useQuery({ query: CampTrainerOverviewDocument, variables: { id } });
  const { rows, days } = React.useMemo(() => {
    const rows = new Map<string, Row>();
    const days = new Map<string, Date>();

    for (
      let day = startOf(new Date(since), 'day');
      day <= new Date(until);
      day = add(day, 1, 'day')
    ) {
      days.set(dayKeyFormatter.format(day), day);
    }
    for (const trainer of query.data?.eventInstance?.trainersList ?? []) {
      rows.set(trainer.personId, {
        id: trainer.personId,
        trainer: trainer.person?.name || '-',
        eventsByDay: new Map(),
        payouts: new Map(),
        payoutIncomplete: false,
      });
    }

    for (const event of query.data?.scheduledEvents ?? []) {
      if (event.type !== 'LESSON' && event.type !== 'GROUP') continue;
      const day = new Date(event.since);
      const dayKey = dayKeyFormatter.format(day);
      days.set(dayKey, day);
      for (const trainer of event.trainersList) {
        const row = rows.get(trainer.personId) ?? {
          id: trainer.personId,
          trainer: trainer.person?.name || '-',
          eventsByDay: new Map(),
          payouts: new Map(),
          payoutIncomplete: false,
        };
        row.eventsByDay.set(dayKey, [...(row.eventsByDay.get(dayKey) ?? []), event]);

        const payout = event.payoutTrainers?.find(
          (candidate) => candidate.personId === trainer.personId,
        );
        if (payout?.memberPayout45MinAmount && payout.currency) {
          const amount =
            (Number(payout.memberPayout45MinAmount) *
              (new Date(event.until).getTime() - new Date(event.since).getTime())) /
            (45 * 60_000);
          row.payouts.set(
            payout.currency,
            (row.payouts.get(payout.currency) ?? 0) + amount,
          );
        } else {
          row.payoutIncomplete = true;
        }
        rows.set(trainer.personId, row);
      }
    }

    return {
      rows: [...rows.values()].toSorted((a, b) =>
        a.trainer.localeCompare(b.trainer, 'cs'),
      ),
      days: [...days].toSorted(([, a], [, b]) => a.getTime() - b.getTime()),
    };
  }, [query.data, since, until]);

  const columns = React.useMemo<Column<Row>[]>(
    () => [
      { key: 'trainer', name: 'Trenér', frozen: true, width: 220 },
      ...days.map<Column<Row>>(([dayKey, day]) => ({
        key: dayKey,
        name: dayFormatter.format(day),
        width: 120,
        renderCell: ({ row }) => (
          <EventCounts events={row.eventsByDay.get(dayKey) ?? []} />
        ),
      })),
      {
        key: 'total',
        name: 'Celkem',
        width: 120,
        renderCell: ({ row }) => (
          <EventCounts events={[...row.eventsByDay.values()].flat()} />
        ),
      },
      {
        key: 'payout',
        name: 'K vyplacení',
        width: 140,
        renderHeaderCell: () => (
          <span title="Podle výchozí sazby trenéra">K vyplacení</span>
        ),
        renderCell: ({ row }) => (
          <div className="flex h-full flex-col justify-center text-right tabular-nums">
            {[...row.payouts].map(([currency, amount]) => (
              <div key={currency}>
                {moneyFormatter.format({ amount: amount.toString(), currency })}
              </div>
            ))}
            {row.payouts.size === 0 && '—'}
            {row.payoutIncomplete && row.payouts.size > 0 && (
              <div className="text-xs text-neutral-10">+ bez sazby</div>
            )}
          </div>
        ),
      },
    ],
    [days],
  );

  return (
    <div className="max-w-full overflow-hidden">
      <FormError error={query.error} />
      {query.fetching && !query.data && <Spinner />}
      {!query.fetching && rows.length === 0 && (
        <div className="py-6 text-sm text-neutral-11">
          Soustředění nemá žádné trenéry.
        </div>
      )}
      {rows.length > 0 && (
        <DataGrid
          columns={columns}
          rows={rows}
          rowKeyGetter={(row) => row.id}
          rowHeight={52}
          headerRowHeight={44}
          style={{ height: Math.min(720, 44 + rows.length * 52) }}
        />
      )}
    </div>
  );
}

function EventCounts({ events }: { events: ScheduledEvent[] }) {
  const lessons = events.filter((event) => event.type === 'LESSON').length;
  const groups = events.length - lessons;
  if (events.length === 0) return <span className="text-neutral-9">—</span>;
  return (
    <div className="flex h-full flex-col justify-center tabular-nums">
      {lessons > 0 && (
        <div>
          {lessons}{' '}
          {lessons <= 4 ? 'lekce' : 'lekcí'}
        </div>
      )}
      {groups > 0 && (
        <div>
          {groups}{' '}
          {groups === 1 ? 'společná' : groups >= 2 && groups <= 4 ? 'společné' : 'společných'}
        </div>
      )}
    </div>
  );
}
