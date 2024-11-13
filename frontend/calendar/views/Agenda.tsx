import type { EventInstanceWithEventFragment } from '@/graphql/Event'
import { EventButton } from '@/ui/EventButton'
import { EventSummary } from '@/ui/EventSummary'
import { formatEventType, formatWeekDay } from '@/ui/format'
import { startOf } from 'date-arithmetic'
import Link from 'next/link'
import React from 'react'
import type { SlotInfo, ViewProps } from '../types'
import { cn } from '@/ui/cn'
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog'
import { UpsertEventForm } from '@/ui/event-form/UpsertEventForm'
import { useAuth } from '@/ui/use-auth'
import { add } from 'date-arithmetic'
import { cardCls } from '@/ui/style'

type MapItem = {
  lessons: Map<string, EventInstanceWithEventFragment[]>;
  groups: EventInstanceWithEventFragment[];
};

function Agenda({ events }: ViewProps): React.ReactNode {
  const dataByDay = React.useMemo(() => {
    const map = new Map<string, MapItem>();
    events.forEach((instance) => {
      const event = instance.event;
      if (!event) return;

      const date = startOf(new Date(instance.since), 'day').toISOString();
      const mapItem: MapItem = map.get(date) ?? { groups: [], lessons: new Map() };
      if (event.type === 'LESSON') {
        const key = event.eventTrainersList.map(x => x.personId).join(',') + event.location?.id + event.locationText;
        mapItem.lessons.set(key, (mapItem.lessons.get(key) ?? []).concat([instance]));
      } else {
        mapItem.groups.push(instance);
      }
      map.set(date, mapItem);
    });
    const list = Array.from(map.entries()).map(([date, itemMap]) => ([
      date,
      {
        groups: itemMap.groups.sort((x, y) => x.since.localeCompare(y.since)),
        lessons: Array.from(itemMap.lessons.entries()).map(([trainers, items]) => {
          items.sort((x, y) => x.since.localeCompare(y.since));
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
            {dateEntry.groups.map(instance => <GroupLesson key={instance.id} instance={instance} />)}
            {dateEntry.lessons.map(([ids, items]) => <LessonGroup key={ids} items={items} />)}
          </div>
        </React.Fragment>
      ))}
    </div>
  );
}

function GroupLesson({ instance }: { instance: EventInstanceWithEventFragment }) {
  return (
    <div className={cardCls({ className: "group min-w-[200px] w-72 pl-3 rounded-lg border-accent-7 border" })}>
      <div className="text-sm text-accent-11">
        {formatEventType(instance.event)}
      </div>
      <Link
        href={`/akce/${instance.event?.id}`}
        className={cn('block mb-2 text-xl', instance.isCancelled ? "line-through" : "underline")}
      >
        {instance.event?.name || instance.event?.eventTrainersList.map(x => x.name).join(', ')}
      </Link>
      <EventSummary instance={instance} />
    </div>
  );
}

function LessonGroup({ items }: { items: EventInstanceWithEventFragment[] }) {
  const auth = useAuth();

  const location = React.useMemo(() => {
    const withLocation = items.find(x => !!x.event?.location?.name || !!x.event?.locationText);
    return withLocation?.event?.location?.name || withLocation?.event?.locationText;
  }, [items]);

  const nextEvent: SlotInfo = React.useMemo(() => {
    const lastEnd = new Date(items[items.length - 1]?.until || '');
    const trainer = items[0]?.event?.eventTrainersList[0]?.personId;
    return {
      start: lastEnd,
      end: add(lastEnd, 45, 'minutes'),
      action: 'click' as const,
      slots: [],
      resource: trainer ? { resourceType: 'person', resourceId: trainer, resourceTitle: '' } : undefined,
    };
  }, [items]);

  return (
    <div className={cardCls({ className: "group min-w-[200px] w-72 pl-1 rounded-lg border-accent-7 border"})}>
      {auth.isTrainerOrAdmin && (
        <Dialog modal={false}>
          <DialogTrigger.Add display="none" variant="none" text="" className="absolute top-1 right-0" />
          <DialogContent className="sm:max-w-xl">
            <UpsertEventForm slot={nextEvent} />
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
        <EventButton key={item.id} instance={item} viewer='trainer' />
      ))}
    </div>
  );
}

export default Agenda
