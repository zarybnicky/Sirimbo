import { EventButton } from '@/ui/EventButton'
import { EventSummary } from '@/ui/EventSummary'
import { datetimeRangeToTimeRange, formatEventType, formatWeekDay } from '@/ui/format'
import { startOf } from 'date-arithmetic'
import Link from 'next/link'
import React from 'react'
import type { CalendarEvent, ViewProps } from '../types'
import { cn } from '@/ui/cn'
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog'
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm'
import { useAuth } from '@/ui/use-auth'
import { add } from 'date-arithmetic'
import { cardCls } from '@/ui/style'
import { TypeOf } from 'zod'
import { EventForm } from '@/ui/event-form/types'

type MapItem = {
  lessons: Map<string, CalendarEvent[]>;
  groups: CalendarEvent[];
};

function Agenda({ events }: ViewProps): React.ReactNode {
  const dataByDay = React.useMemo(() => {
    const map = new Map<string, MapItem>();
    for (const calendarEvent of events) {
      const { event, instance } = calendarEvent;

      const date = startOf(new Date(instance.since), 'day').toISOString();
      const mapItem: MapItem = map.get(date) ?? { groups: [], lessons: new Map() };
      if (event.type === 'LESSON') {
        const key = event.eventTrainersList.map(x => x.personId).join(',') + event.location?.id + event.locationText;
        mapItem.lessons.set(key, [...(mapItem.lessons.get(key) ?? []), calendarEvent]);
      } else {
        mapItem.groups.push(calendarEvent);
      }
      map.set(date, mapItem);
    }
    const list = [...map.entries()].map(([date, itemMap]) => ([
      date,
      {
        groups: itemMap.groups.sort((x, y) => x.start.getTime() - y.start.getTime()),
        lessons: [...itemMap.lessons.entries()].map(([trainers, items]) => {
          items.sort((x, y) => x.start.getTime() - y.start.getTime());
          return [trainers, items] as const;
        }).sort((x, y) => x[0].localeCompare(y[0])),
      }
    ] as const));
    return list.sort((x, y) => x[0].localeCompare(y[0]));
  }, [events]);

  return (
    <div className="col-full-width p-4 lg:pb-8 overflow-y-auto overscroll-contain">
      {!events?.length && (
        <div className="border border-accent-6 p-2 bg-accent-1 text-accent-12 rounded-md">
          Žádné tréninky pro tento týden
        </div>
      )}

      {dataByDay.map(([date, dateEntry], i) => (
        <React.Fragment key={i}>
          <div className="text-2xl tracking-wide mt-8 mb-2">
            {formatWeekDay(new Date(date))}
          </div>

          <div className="flex justify-start flex-wrap gap-2 ml-2 pl-5 border-l-4 border-accent-10">
            {dateEntry.groups.map(calendarEvent => <GroupLesson key={calendarEvent.instance.id} calendarEvent={calendarEvent} />)}
            {dateEntry.lessons.map(([ids, items]) => <LessonGroup key={ids} items={items} />)}
          </div>
        </React.Fragment>
      ))}
    </div>
  );
}

function GroupLesson({ calendarEvent }: {
  calendarEvent: CalendarEvent;
}) {
  const { event, instance } = calendarEvent;
  return (
    <div className={cardCls({ className: "group min-w-[200px] w-72 pl-3 rounded-lg border-accent-7 border" })}>
      <div className="text-sm text-accent-11">
        {formatEventType(event)}
      </div>
      <Link
        href={{ pathname: '/akce/[id]', query: { id: event.id } }}
        className={cn('block mb-2 text-xl', instance.isCancelled ? "line-through" : "underline")}
      >
        {event.name || event.eventTrainersList.map(x => x.name).join(', ')}
      </Link>
      <EventSummary event={event} instance={instance} />
    </div>
  );
}

function LessonGroup({ items }: { items: CalendarEvent[] }) {
  const auth = useAuth();

  const location = React.useMemo(() => {
    const withLocation = items.find(x => !!x.event?.location?.name || !!x.event?.locationText);
    return withLocation?.event?.location?.name || withLocation?.event?.locationText;
  }, [items]);

  const nextEvent: Partial<TypeOf<typeof EventForm>> = React.useMemo(() => {
    const lastEnd = items.at(-1)?.end ?? new Date();
    const trainer = items[0]?.event?.eventTrainersList[0]?.personId;
    return {
      instances: [{
        ...datetimeRangeToTimeRange(lastEnd, add(lastEnd, 45, 'minutes')),
        isCancelled: false,
      }],
      isVisible: true,
      type: 'LESSON',
      capacity: 2,
      locationId: items[0]?.event?.location?.id,
      locationText: items[0]?.event?.locationText,
      trainers: trainer ? [{ itemId: null, personId: trainer, lessonsOffered: 0 }] : [],
    };
  }, [items]);

  return (
    <div className={cardCls({ className: "group min-w-[200px] w-72 pl-1 rounded-lg border-accent-7 border" })}>
      {auth.isTrainerOrAdmin && (
        <Dialog modal={false}>
          <DialogTrigger.Add display="none" variant="none" text="" className="absolute top-1 right-0" />
          <DialogContent className="sm:max-w-xl" onOpenAutoFocus={preventDefault}>
            <UpsertEventForm initialValue={nextEvent} />
          </DialogContent>
        </Dialog>
      )}

      <div className="ml-3 text-sm text-accent-11">
        {location}
      </div>
      <div className="ml-3 text-xl mb-1">
        {items[0]?.event?.eventTrainersList.map(x => x.name).join(', ')}
      </div>
      {items.map((item) => (
        <EventButton key={item.instance.id} event={item.event} instance={item.instance} viewer='trainer' />
      ))}
    </div>
  );
}

export default Agenda

const preventDefault = (e: Event) => e.preventDefault();
