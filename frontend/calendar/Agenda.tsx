import { EventButton } from '@/ui/EventButton';
import { EventSummary } from '@/ui/EventSummary';
import { formatEventType, formatWeekDay } from '@/ui/format';
import { add, startOf } from 'date-arithmetic';
import Link from 'next/link';
import React from 'react';
import type { CalendarEvent, ViewProps } from '@/calendar/types';
import { cn } from '@/lib/cn';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm';
import { useAuth } from '@/ui/use-auth';
import { cardCls } from '@/ui/style';
import { EventFormType } from '@/ui/event-form/types';
import { isTruthy } from '@/lib/truthyFilter';
import { ConflictsInstanceBadge } from '@/calendar/ConflictsInstanceBadge';

type MapItem = {
  lessons: Map<string, CalendarEvent[]>;
  groups: CalendarEvent[];
};

const preventDefault = (e: Event) => e.preventDefault();

function Agenda({ events }: ViewProps): React.ReactNode {
  const dataByDay = React.useMemo(() => {
    const map = new Map<string, MapItem>();
    for (const calendarEvent of events) {
      const { instance } = calendarEvent;

      const date = startOf(new Date(instance.since), 'day').toISOString();
      const mapItem: MapItem = map.get(date) ?? { groups: [], lessons: new Map() };
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

      {dataByDay.map(([date, dateEntry]) => (
        <React.Fragment key={date}>
          <div className="text-2xl tracking-wide mt-8 mb-2">
            {formatWeekDay(new Date(date))}
          </div>

          <div className="flex justify-start flex-wrap gap-2">
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
        </React.Fragment>
      ))}
    </div>
  );
}

function GroupLesson({ calendarEvent }: { calendarEvent: CalendarEvent }) {
  const { event, instance } = calendarEvent;

  return (
    <div
      className={cardCls({
        className:
          'relative group min-w-[200px] w-72 rounded-lg border-accent-7 border' +
          (event.eventTargetCohortsList.length > 0 ? ' pl-5' : ' pl-3'),
      })}
    >
      <ConflictsInstanceBadge
        instanceId={instance.id}
        className="absolute right-8 top-3 text-accent-11"
      />
      {event.eventTargetCohortsList.length > 0 && (
        <div className="absolute rounded-l-lg overflow-hidden opacity-80 border-r border-neutral-6 shadow-sm inset-y-0 left-0 flex flex-col">
          {event.eventTargetCohortsList
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
        href={{ pathname: '/akce/[id]', query: { id: instance.eventId } }}
        className={cn(
          'block mb-2 text-xl',
          instance.isCancelled ? 'line-through' : 'underline',
        )}
      >
        {instance.name ||
          (instance.trainersList ?? []).map((x) => x.person?.name).join(', ')}
      </Link>
      <EventSummary event={event} instance={instance} />
    </div>
  );
}

function LessonGroup({ items }: { items: CalendarEvent[] }) {
  const auth = useAuth();

  const locLabel = (it: CalendarEvent) =>
    it.instance.location?.name || it.instance.locationText || '-';

  const nextEvent: Partial<EventFormType> = React.useMemo(() => {
    const last = items.at(-1);
    const lastEnd = last?.end ?? new Date();
    return {
      instances: [
        {
          itemId: null,
          since: lastEnd.toISOString(),
          until: add(lastEnd, 45, 'minutes').toISOString(),
          isCancelled: false,
          trainers: [],
        },
      ],
      isVisible: true,
      type: 'LESSON',
      capacity: 2,
      locationId: last?.instance?.location?.id,
      locationText: last?.instance?.locationText,
      trainers:
        last?.instance.trainersList?.map(({ personId }) => ({
          itemId: null,
          personId,
          lessonsOffered: 0,
        })) || [],
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
            <UpsertEventForm initialValue={nextEvent} />
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
            <EventButton event={item.event} instance={item.instance} viewer="trainer" />
          </React.Fragment>
        );
      })}
    </div>
  );
}

export default Agenda;
