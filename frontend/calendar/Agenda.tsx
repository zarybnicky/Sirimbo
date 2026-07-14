import { EventButton } from '@/ui/EventButton';
import { EventSummary } from '@/ui/EventSummary';
import { capitalize, formatEventType, weekDayFormatter } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import Link from 'next/link';
import React from 'react';
import type {
  CalendarBirthdayEvent,
  CalendarCompetitionEvent,
  ViewProps,
} from '@/calendar/types';
import { Cake } from 'lucide-react';
import { cn } from '@/lib/cn';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { EventCreateForm } from '@/ui/event-form/EventForms';
import { useAuth } from '@/ui/use-auth';
import { CompetitionEventContent } from '@/ui/Competitions';
import { cardCls } from '@/ui/style';
import { isTruthy } from '@/lib/truthyFilter';
import { ConflictsInstanceBadge } from '@/calendar/ConflictsInstanceBadge';
import type { EventWithTrainerFragment } from '@/graphql/Event';

type MapItem = {
  birthdays: CalendarBirthdayEvent[];
  competitions: CalendarCompetitionEvent[];
  lessons: Map<string, EventWithTrainerFragment[]>;
  groups: EventWithTrainerFragment[];
};

const preventDefault = (e: Event) => e.preventDefault();

function Agenda({ events }: ViewProps): React.ReactNode {
  const dataByDay = React.useMemo(() => {
    const map = new Map<string, MapItem>();
    for (const calendarEvent of events) {
      const date = calendarEvent.start.toISOString().slice(0, 10);
      const mapItem: MapItem = map.get(date) ?? {
        birthdays: [],
        competitions: [],
        groups: [],
        lessons: new Map(),
      };

      if (calendarEvent.kind === 'birthday') {
        mapItem.birthdays.push(calendarEvent);
      } else if (calendarEvent.kind === 'competition') {
        mapItem.competitions.push(calendarEvent);
      } else {
        const { instance } = calendarEvent;
        if (instance.type === 'LESSON') {
          const key =
            instance.trainersList
              ?.map((x) => x.personId)
              .toSorted()
              .join(',') ?? '';
          const arr = mapItem.lessons.get(key);
          if (arr) arr.push(calendarEvent.instance);
          else mapItem.lessons.set(key, [calendarEvent.instance]);
        } else {
          mapItem.groups.push(calendarEvent.instance);
        }
      }
      map.set(date, mapItem);
    }
    const list = [...map.entries()].map(
      ([date, itemMap]) =>
        [
          date,
          {
            birthdays: itemMap.birthdays.toSorted(
              (x, y) =>
                x.person.lastName.localeCompare(y.person.lastName) ||
                x.person.firstName.localeCompare(y.person.firstName),
            ),
            competitions: itemMap.competitions,
            groups: itemMap.groups.toSorted((x, y) => x.since.localeCompare(y.since)),
            lessons: [...itemMap.lessons.entries()]
              .map(
                ([trainers, items]) =>
                  [
                    trainers,
                    items.toSorted((x, y) => x.since.localeCompare(y.since)),
                  ] as const,
              )
              .toSorted((x, y) => x[0].localeCompare(y[0])),
          },
        ] as const,
    );
    return list.toSorted((x, y) => x[0].localeCompare(y[0]));
  }, [events]);

  return (
    <div className="col-full-width p-4 lg:pb-8 overflow-y-auto overscroll-contain">
      {!events?.length && (
        <div className="border border-accent-6 p-2 bg-accent-1 text-accent-12 rounded-md">
          Žádné tréninky pro tento týden
        </div>
      )}

      {dataByDay.map(([date, dateEntry]) => (
        <React.Fragment key={date}>
          <div className="mt-8 mb-2 flex flex-wrap items-center gap-x-3 gap-y-2">
            <div className="text-2xl tracking-wide">
              {capitalize(weekDayFormatter.format(new Date(date)))}
            </div>
            {dateEntry.birthdays.map((calendarEvent) => (
              <BirthdayChip key={calendarEvent.id} calendarEvent={calendarEvent} />
            ))}
          </div>

          <div className="flex justify-start flex-wrap gap-2 opacity-90">
            {dateEntry.competitions.map((calendarEvent) => (
              <div
                key={calendarEvent.id}
                className={cardCls({
                  className:
                    'min-w-[200px] w-72 rounded-lg border-green-7 bg-green-2 p-3',
                })}
              >
                <CompetitionEventContent
                  title={calendarEvent.title}
                  location={calendarEvent.eventLocation}
                  entries={calendarEvent.items}
                />
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
      className="inline-flex max-w-full items-center gap-1 rounded border border-neutral-6 bg-neutral-2 px-2 py-1 text-sm text-neutral-12 underline-offset-2 hover:bg-neutral-3 hover:underline"
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
          'relative group min-w-[200px] w-72 rounded-lg border-accent-7 border' +
          (instance.targetCohortsList && instance.targetCohortsList.length > 0
            ? ' pl-5'
            : ' pl-3'),
      })}
    >
      <ConflictsInstanceBadge
        instanceId={instance.id}
        className="absolute right-8 top-3 text-accent-11"
      />
      {instance.targetCohortsList && instance.targetCohortsList.length > 0 && (
        <div className="absolute rounded-l-lg overflow-hidden opacity-80 border-r border-neutral-6 shadow-sm inset-y-0 left-0 flex flex-col">
          {instance.targetCohortsList
            .map((x) => x.cohort?.colorRgb)
            .filter(isTruthy)
            .map((c) => (
              <div key={c} className="flex-1 w-2" style={{ backgroundColor: c }} />
            ))}
        </div>
      )}
      <div className="text-sm text-accent-11">{formatEventType(instance.type)}</div>
      <Link
        href={`/termin/${instance.id}`}
        className={cn(
          'block mb-2 text-xl',
          instance.isCancelled ? 'line-through' : 'underline',
        )}
      >
        {instance.name ||
          (instance.trainersList ?? []).map((x) => x.person?.name).join(', ')}
      </Link>
      <EventSummary instance={instance} />
    </div>
  );
}

function LessonGroup({ items }: { items: EventWithTrainerFragment[] }) {
  const auth = useAuth();

  const nextEventDefaults = React.useMemo(() => {
    const last = items.at(-1);
    const since = last ? new Date(last.until) : new Date();

    return {
      since,
      until: add(since, 45, 'minutes'),
      trainerPersonIds: last?.trainersList?.map(({ personId }) => personId) ?? [],
      locationId: last?.location?.id ?? null,
      locationText: last?.locationText ?? '',
    };
  }, [items]);

  return (
    <div
      className={cardCls({
        className:
          'relative group min-w-[200px] w-72 p-1 pt-2 rounded-lg border-accent-7 border flex flex-col gap-y-px',
      })}
    >
      {auth.isTrainerOrAdmin && (
        <Dialog modal={false}>
          <DialogTrigger.Add
            display="none"
            variant="none"
            text=""
            className="absolute top-1 right-0"
          />
          <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
            <EventCreateForm defaults={nextEventDefaults} />
          </DialogContent>
        </Dialog>
      )}

      <div className="ml-2 text-xl mb-1">
        {items[0]?.trainersList?.map((x) => x.person?.name).join(', ') ?? ''}
      </div>

      {items.map((instance, i) => {
        const loc = instance.location?.name || instance.locationText;
        const prev = i === 0 ? null : items.at(i - 1);
        const prevLoc = prev ? prev.location?.name || prev.locationText : null;

        return (
          <React.Fragment key={instance.id}>
            {loc !== prevLoc && i !== 0 && !!loc && (
              <div className="ml-2.5 mt-1 text-sm text-accent-11">{loc}</div>
            )}
            <EventButton instance={instance} viewer="trainer" />
          </React.Fragment>
        );
      })}
    </div>
  );
}

export default Agenda;
