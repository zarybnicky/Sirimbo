import { EventButton } from '@/ui/EventButton';
import { EventSummary } from '@/ui/EventSummary';
import { formatEventType, formatWeekDay } from '@/ui/format';
import { startOf } from 'date-arithmetic';
import Link from 'next/link';
import React from 'react';
import type {
  CalendarBirthdayEvent,
  CalendarCompetitionEvent,
  CalendarInstanceEvent,
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
import { localDateKey } from '@/calendar/localizer';
import { quickDefaultsAfterLessonGroup } from '@/calendar/quickEventDefaults';

type MapItem = {
  birthdays: CalendarBirthdayEvent[];
  competitions: CalendarCompetitionEvent[];
  lessons: Map<string, CalendarInstanceEvent[]>;
  groups: CalendarInstanceEvent[];
};

const preventDefault = (e: Event) => e.preventDefault();

function Agenda({ events }: ViewProps): React.ReactNode {
  const dataByDay = React.useMemo(() => {
    const map = new Map<string, MapItem>();
    for (const calendarEvent of events) {
      const date = localDateKey(startOf(calendarEvent.start, 'day'));
      const mapItem: MapItem = map.get(date) ?? {
        birthdays: [],
        competitions: [],
        groups: [],
        lessons: new Map(),
      };

      if (calendarEvent.kind === 'birthday') {
        mapItem.birthdays.push(calendarEvent);
        map.set(date, mapItem);
        continue;
      }

      if (calendarEvent.kind === 'competition') {
        mapItem.competitions.push(calendarEvent);
        map.set(date, mapItem);
        continue;
      }

      const { instance } = calendarEvent;
      if (instance.type === 'LESSON') {
        const trainers = instance.trainersList ?? [];
        const key = trainers
          .map((x) => x.personId)
          .toSorted((a, b) => a.localeCompare(b))
          .join(',');
        const arr = mapItem.lessons.get(key);
        if (arr) arr.push(calendarEvent);
        else mapItem.lessons.set(key, [calendarEvent]);
      } else {
        mapItem.groups.push(calendarEvent);
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
            groups: itemMap.groups.toSorted(
              (x, y) => x.start.getTime() - y.start.getTime(),
            ),
            lessons: [...itemMap.lessons.entries()]
              .map(
                ([trainers, items]) =>
                  [
                    trainers,
                    items.toSorted((x, y) => x.start.getTime() - y.start.getTime()),
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

      {dataByDay.map(([date, dateEntry]) => {
        const hasEventCards =
          dateEntry.competitions.length > 0 ||
          dateEntry.groups.length > 0 ||
          dateEntry.lessons.length > 0;

        return (
          <React.Fragment key={date}>
            <div className="mt-8 mb-2 flex flex-wrap items-center gap-x-3 gap-y-2">
              <div className="text-2xl tracking-wide">
                {formatWeekDay(new Date(`${date}T00:00:00`))}
              </div>
              {dateEntry.birthdays.map((calendarEvent) => (
                <BirthdayChip key={calendarEvent.id} calendarEvent={calendarEvent} />
              ))}
            </div>

            {hasEventCards && (
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
                {dateEntry.groups.map((calendarEvent) => (
                  <GroupLesson
                    key={calendarEvent.instance.id}
                    calendarEvent={calendarEvent}
                  />
                ))}
                {dateEntry.lessons.map(([ids, items]) => (
                  <LessonGroup key={ids} items={items} />
                ))}
              </div>
            )}
          </React.Fragment>
        );
      })}
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

function GroupLesson({ calendarEvent }: { calendarEvent: CalendarInstanceEvent }) {
  const { instance } = calendarEvent;

  return (
    <div
      className={cardCls({
        className:
          'relative group min-w-[200px] w-72 rounded-lg border-accent-7 border' +
          (instance.targetCohortsList && instance.targetCohortsList.length > 0 ? ' pl-5' : ' pl-3'),
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
            .map((color) => (
              <div
                key={color}
                className="flex-1 w-2"
                style={{ backgroundColor: color }}
              />
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

function LessonGroup({ items }: { items: CalendarInstanceEvent[] }) {
  const auth = useAuth();

  const locLabel = (it: CalendarInstanceEvent) =>
    it.instance.location?.name || it.instance.locationText || '-';

  const nextEventDefaults = React.useMemo(
    () => quickDefaultsAfterLessonGroup(items),
    [items],
  );

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
        {(items[0]?.instance.trainersList ?? []).map((x) => x.person?.name).join(', ') ||
          '-'}
      </div>

      {items.map((item, i) => {
        const loc = locLabel(item);
        const prevLoc = i === 0 ? null : locLabel(items[i - 1]!);
        const showHeader = loc !== prevLoc && (i !== 0 || loc !== '-'); // Don't show first if missing

        return (
          <React.Fragment key={item.instance.id}>
            {showHeader && (
              <div className="ml-2.5 mt-1 text-sm text-accent-11">{loc}</div>
            )}
            <EventButton instance={item.instance} viewer="trainer" />
          </React.Fragment>
        );
      })}
    </div>
  );
}

export default Agenda;
