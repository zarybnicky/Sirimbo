import {
  CampTrainerOverviewDocument,
  type CampTrainerOverviewQuery,
} from '@/graphql/Event';
import { cn } from '@/lib/cn';
import { compareText } from '@/lib/compare';
import { dayFormatter, formatCount, moneyFormatter } from '@/ui/format';
import { Table } from '@/ui/Table';
import { add, startOf } from 'date-arithmetic';
import * as React from 'react';
import { useQuery } from 'urql';

type ScheduledEvent = NonNullable<CampTrainerOverviewQuery['eventsList']>[number];
type Row = {
  id: string;
  trainer: string;
  eventsByDay: Map<string, ScheduledEvent[]>;
  payouts: Map<string, number>;
  payoutIncomplete: boolean;
};

const dayKeyFormatter = new Intl.DateTimeFormat('sv-SE');

export const CampTrainersTable = React.memo(function CampTrainersTable({
  id,
}: {
  id: string;
}) {
  const [query] = useQuery({ query: CampTrainerOverviewDocument, variables: { id } });
  const { rows, days } = React.useMemo(() => {
    const rows = new Map<string, Row>();
    const days = new Map<string, Date>();

    if (!query.data?.event) return { rows: [], days: [] };
    const event = query.data.event;

    for (
      let day = startOf(new Date(event.since), 'day');
      day <= new Date(event.until);
      day = add(day, 1, 'day')
    ) {
      days.set(dayKeyFormatter.format(day), day);
    }
    for (const trainer of event.eventTrainersList) {
      rows.set(trainer.personId, {
        id: trainer.personId,
        trainer: trainer.person?.name || '-',
        eventsByDay: new Map(),
        payouts: new Map(),
        payoutIncomplete: false,
      });
    }

    for (const event of query.data.eventsList ?? []) {
      if (event.type !== 'LESSON' && event.type !== 'GROUP') continue;
      const day = new Date(event.since);
      const dayKey = dayKeyFormatter.format(day);
      days.set(dayKey, day);
      for (const trainer of event.eventTrainersList) {
        const row = rows.get(trainer.personId) ?? {
          id: trainer.personId,
          trainer: trainer.person?.name || '-',
          eventsByDay: new Map(),
          payouts: new Map(),
          payoutIncomplete: false,
        };
        row.eventsByDay.set(dayKey, [...(row.eventsByDay.get(dayKey) ?? []), event]);

        const payout = trainer.tenantTrainer;
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
      rows: [...rows.values()].toSorted((a, b) => compareText(a.trainer, b.trainer)),
      days: [...days].toSorted(([, a], [, b]) => a.getTime() - b.getTime()),
    };
  }, [query.data]);

  return (
    <Table
      className={cn(
        'max-h-180 overflow-auto',
        '[&_thead]:sticky [&_thead]:top-0 [&_thead]:z-20 [&_thead]:bg-neutral-1',
        '[&_tr>:first-child]:sticky [&_tr>:first-child]:left-0 [&_tr>:first-child]:z-10 [&_tr>:first-child]:bg-neutral-1',
      )}
      columns={[
        'Trenér',
        ...days.map(([dayKey, day]) => (
          <span key={dayKey} className="whitespace-nowrap">
            {dayFormatter.format(day)}
          </span>
        )),
        'Celkem',
        <span key="payout" className="block text-right">
          K vyplacení
        </span>,
      ]}
      busy={query.fetching}
      error={query.error}
    >
      {rows.map((row) => (
        <tr key={row.id} className="border-t border-neutral-4 align-middle">
          <th scope="row">{row.trainer}</th>
          {days.map(([dayKey]) => (
            <td key={dayKey}>
              <EventCounts events={row.eventsByDay.get(dayKey) ?? []} />
            </td>
          ))}
          <td>
            <EventCounts events={[...row.eventsByDay.values()].flat()} />
          </td>
          <td className="text-right">
            {[...row.payouts].map(([currency, amount]) => (
              <div key={currency} className="whitespace-nowrap">
                {moneyFormatter.format({ amount, currency })}
              </div>
            ))}
            {row.payoutIncomplete && row.payouts.size > 0 && (
              <div className="text-xs text-neutral-10">+ trenér bez hodinovky</div>
            )}
          </td>
        </tr>
      ))}
    </Table>
  );
});

function EventCounts({ events }: { events: ScheduledEvent[] }) {
  const lessons = events.filter((event) => event.type === 'LESSON').length;
  const groups = events.length - lessons;
  if (events.length === 0) return null;
  return (
    <div className="flex flex-col tabular-nums">
      {lessons > 0 && (
        <div>{formatCount(lessons, { one: 'lekce', few: 'lekce', other: 'lekcí' })}</div>
      )}
      {groups > 0 && (
        <div>
          {formatCount(groups, {
            one: 'společná',
            few: 'společné',
            other: 'společných',
          })}
        </div>
      )}
    </div>
  );
}
