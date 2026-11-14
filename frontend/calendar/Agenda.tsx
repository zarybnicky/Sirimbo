import { EventConflictsBadge } from '@/calendar/EventConflictsBadge';
import type { ViewProps } from '@/calendar/types';
import type { EventWithTrainerFragment } from '@/graphql/Event';
import { CompetitionEventCard } from '@/ui/Competitions';
import { EventButton } from '@/ui/EventButton';
import { EventSummary } from '@/ui/EventSummary';
import { BirthdayChip } from '@/ui/BirthdayChip';
import { capitalize, listFormatter, longDayFormatter } from '@/ui/format';
import { buttonCls, cardCls } from '@/ui/style';
import { useAuth } from '@/lib/auth';
import { startOf } from 'date-arithmetic';
import { Plus } from 'lucide-react';
import React from 'react';
import { useAtomValue } from 'jotai';
import { dragListenersAtom } from './state';

function Agenda({ events }: ViewProps): React.ReactNode {
  const auth = useAuth();
  const { onCreate } = useAtomValue(dragListenersAtom);

  const days = React.useMemo(() => {
    return [...Map.groupBy(events, (x) => +startOf(x.start, 'day'))]
      .toSorted(([a], [b]) => a - b)
      .map(([date, items]) => {
        const events = items.filter((x) => x.kind === 'event').map((x) => x.instance);
        return {
          date,
          events,
          competitions: items.filter((x) => x.kind === 'competition'),
          birthdays: items.filter((x) => x.kind === 'birthday'),
          groups: events.filter((e) => e.type !== 'LESSON'),
          lessons: [
            ...Map.groupBy(
              events.filter((e) => e.type === 'LESSON'),
              (e) =>
                e.eventTrainersList
                  .map((x) => x.personId)
                  .toSorted()
                  .join(','),
            ),
          ].toSorted((x, y) => x[0].localeCompare(y[0])),
        };
      });
  }, [events]);

  return (
    <div className="col-full-width p-4 lg:pb-8">
      {!events?.length && (
        <div className="border border-accent-6 p-2 bg-accent-1 text-accent-12 rounded-md">
          Žádné tréninky pro tento týden
        </div>
      )}

      {days.map(({ date, events, groups, lessons, competitions, birthdays }) => (
        <React.Fragment key={date}>
          <div className="mt-8 mb-2 flex flex-wrap items-center gap-x-3 gap-y-2">
            <div className="flex items-center text-xl tracking-tight">
              {capitalize(longDayFormatter.format(new Date(date)))}
              {auth.isTrainer && (
                <button
                  type="button"
                  className={buttonCls({ display: 'none', variant: 'none' })}
                  onClick={() => onCreate?.({ date: new Date(date), after: events.at(-1) })}
                >
                  <Plus />
                </button>
              )}
            </div>
            {birthdays.map((x) => (
              <BirthdayChip key={x.id} person={x.person} />
            ))}
          </div>

          <div className="flex justify-start items-start flex-wrap gap-2 opacity-90">
            {competitions.length > 0 && (
              <div className="flex w-full flex-col gap-2 sm:w-72">
                {competitions.map((x) => (
                  <CompetitionEventCard key={x.id} {...x} />
                ))}
              </div>
            )}
            {groups.map((x) => (
              <GroupLesson key={x.id} instance={x} />
            ))}
            {lessons.map(([ids, items]) => (
              <LessonGroup key={ids} items={items} />
            ))}
          </div>
        </React.Fragment>
      ))}
    </div>
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
        className="absolute right-3 top-3 text-accent-11"
      />
      {!!instance.targetCohortsList?.length && (
        <div className="absolute rounded-l-lg overflow-hidden opacity-80 border-r border-neutral-6 shadow-xs inset-y-0 left-0 flex flex-col">
          {instance.targetCohortsList
            .flatMap((x) => x.cohort?.colorRgb || [])
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
          'relative group min-w-50 w-full sm:w-72 p-1 pt-2 rounded-lg border-accent-7 border flex flex-col gap-y-px',
      })}
    >
      {auth.isTrainer && (
        <button
          type="button"
          title="Přidat lekci"
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
          (items[0]?.eventTrainersList ?? []).flatMap((x) => x.person?.name ?? []),
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
