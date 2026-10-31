import { EventConflictsBadge } from '@/calendar/EventConflictsBadge';
import type { CalendarBirthdayEvent, ViewProps } from '@/calendar/types';
import type { EventWithTrainerFragment } from '@/graphql/Event';
import { isTruthy } from '@/lib/truthyFilter';
import { compareText } from '@/lib/compare';
import { CompetitionEventContent } from '@/ui/Competitions';
import { EventButton } from '@/ui/EventButton';
import { EventSummary } from '@/ui/EventSummary';
import {
  capitalize,
  fullDateFormatter,
  listFormatter,
  longDayFormatter,
} from '@/ui/format';
import { buttonCls, cardCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { startOf } from 'date-arithmetic';
import { Cake, Plus } from 'lucide-react';
import Link from 'next/link';
import React from 'react';
import { useAtomValue } from 'jotai'; import { dragListenersAtom } from './state';

function Agenda({ events }: ViewProps): React.ReactNode {
  const auth = useAuth();
  const { onCreate } = useAtomValue(dragListenersAtom);

  const dataByDay = React.useMemo(() => {
    return [...Map.groupBy(events, (e) => +startOf(e.start, 'day'))]
      .toSorted(([x], [y]) => x - y)
      .map(([date, items]) => {
        const events = items.filter((e) => e.kind === 'event').map((e) => e.instance);
        return [
          date,
          {
            birthdays: items
              .filter((e) => e.kind === 'birthday')
              .toSorted(
                (x, y) =>
                  compareText(x.person.lastName, y.person.lastName) ||
                  compareText(x.person.firstName, y.person.firstName),
              ),
            competitions: items.filter((e) => e.kind === 'competition'),
            groups: events
              .filter((e) => e.type !== 'LESSON')
              .toSorted((x, y) => x.since.localeCompare(y.since)),
            lessons: [
              ...Map.groupBy(
                events.filter((e) => e.type === 'LESSON'),
                (e) => e.eventTrainersList.map((x) => x.personId).toSorted().join(','),
              ),
            ]
              .map(
                ([trainers, items]) =>
                  [
                    trainers,
                    items.toSorted((x, y) => x.since.localeCompare(y.since)),
                  ] as const,
              )
              .toSorted((x, y) => x[0].localeCompare(y[0])),
          },
        ] as const;
      });
  }, [events]);

  return (
    <div className="col-full-width p-4 lg:pb-8">
      {!events?.length && (
        <div className="border border-accent-6 p-2 bg-accent-1 text-accent-12 rounded-md">
          Žádné tréninky pro tento týden
        </div>
      )}

      {dataByDay.map(([date, dateEntry]) => (
        <React.Fragment key={date}>
          <div className="mt-8 mb-2 flex flex-wrap items-center gap-x-3 gap-y-2">
            <div className="flex items-center text-xl tracking-tight">
              {capitalize(longDayFormatter.format(new Date(date)))}
              {auth.isTrainer && (
                <button
                  type="button"
                  aria-label={`Přidat událost na ${fullDateFormatter.format(new Date(date))}`}
                  className={buttonCls({ display: 'none', variant: 'none' })}
                  onClick={() => onCreate?.({ date: new Date(date) })}
                >
                  <Plus />
                </button>
              )}
            </div>
            {dateEntry.birthdays.map((calendarEvent) => (
              <BirthdayChip key={calendarEvent.id} calendarEvent={calendarEvent} />
            ))}
          </div>

          <div className="flex justify-start items-start flex-wrap gap-2 opacity-90">
            {dateEntry.competitions.map((calendarEvent) => (
              <div
                key={calendarEvent.id}
                className={cardCls({
                  className:
                    'min-w-[200px] w-full sm:w-72 rounded-lg border-green-7 bg-green-2 p-3',
                })}
              >
                <CompetitionEventContent entries={calendarEvent.items} />
              </div>
            ))}
            {dateEntry.groups.map((instance) => (
              <GroupLesson key={instance.id} instance={instance} />
            ))}
            {dateEntry.lessons.map(([ids, items]) => (
              <LessonGroup key={ids} items={items} />
            ))}
          </div>
        </React.Fragment>
      ))}
    </div>
  );
}

function BirthdayChip({ calendarEvent }: { calendarEvent: CalendarBirthdayEvent }) {
  return (
    <Link
      href={`/clenove/${calendarEvent.person.id}`}
      title={`Narozeniny: ${calendarEvent.person.name}`}
      className="inline-flex max-w-full items-center gap-1 rounded-sm border border-neutral-6 bg-neutral-2 px-2 py-1 text-xs text-neutral-12 underline-offset-2 hover:bg-neutral-3 hover:underline"
    >
      <Cake className="size-3.5 shrink-0 text-accent-11" />
      <span className="truncate">{calendarEvent.person.name}</span>
    </Link>
  );
}

function GroupLesson({ instance }: { instance: EventWithTrainerFragment }) {
  return (
    <div
      className={cardCls({
        className:
          'relative group min-w-[200px] w-full sm:w-72 rounded-lg border-accent-7 border' +
          (instance.targetCohortsList && instance.targetCohortsList.length > 0
            ? ' pl-5'
            : ' pl-3'),
      })}
    >
      <EventConflictsBadge
        eventId={instance.id}
        className="absolute right-8 top-3 text-accent-11"
      />
      {instance.targetCohortsList && instance.targetCohortsList.length > 0 && (
        <div className="absolute rounded-l-lg overflow-hidden opacity-80 border-r border-neutral-6 shadow-xs inset-y-0 left-0 flex flex-col">
          {instance.targetCohortsList
            .map((x) => x.cohort?.colorRgb)
            .filter(isTruthy)
            .map((c) => (
              <div key={c} className="flex-1 w-2" style={{ backgroundColor: c }} />
            ))}
        </div>
      )}
      <EventSummary event={instance} />
    </div>
  );
}

function LessonGroup({ items }: { items: EventWithTrainerFragment[] }) {
  const auth = useAuth();
  const { onCreate } = useAtomValue(dragListenersAtom);

  return (
    <div
      className={cardCls({
        className:
          'relative group min-w-[200px] w-full sm:w-72 p-1 pt-2 rounded-lg border-accent-7 border flex flex-col gap-y-px',
      })}
    >
      {auth.isTrainer && (
        <button
          type="button"
          aria-label="Přidat lekci"
          className={buttonCls({
            display: 'none',
            variant: 'none',
            className: 'absolute top-1 right-0',
          })}
          onClick={() => onCreate?.({ after: items.at(-1) })}
        >
          <Plus />
        </button>
      )}

      <div className="ml-2 text-xl mb-1">
        {listFormatter.format(
          (items[0]?.eventTrainersList ?? []).map((x) => x.person?.name).filter(isTruthy),
        )}
      </div>

      {items.map((instance, i) => {
        const loc = instance.location?.name || instance.locationText;
        const prev = i === 0 ? null : items.at(i - 1);
        const prevLoc = prev ? prev.location?.name || prev.locationText : null;

        return (
          <React.Fragment key={instance.id}>
            {loc !== prevLoc && !!loc && (
              <div className="ml-2.5 not-first:mt-1 text-sm text-accent-11">{loc}</div>
            )}
            <EventButton event={instance} viewer="trainer" />
          </React.Fragment>
        );
      })}
    </div>
  );
}

export default Agenda;
