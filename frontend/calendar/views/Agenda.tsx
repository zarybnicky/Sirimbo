import { EventInstanceWithEventFragment } from '@/graphql/Event'
import { Card } from '@/ui/Card'
import { EventButton } from '@/ui/EventButton'
import { EventSummary } from '@/ui/EventSummary'
import { formatEventType, formatWeekDay } from '@/ui/format'
import { startOf } from 'date-arithmetic'
import Link from 'next/link'
import React from 'react'
import { ViewClass } from '../types'

const Agenda: ViewClass = ({ events }) => {
  const dataByDay = React.useMemo(() => {
    const map = new Map<string, Map<string, EventInstanceWithEventFragment[]>>();
    events.forEach((instance) => {
      const date = startOf(new Date(instance.since), 'day').toISOString();
      const event = instance.event;
      const trainers = !event ? '' : event.type === 'LESSON' ? event.eventTrainersList.map(x => x.personId).join(',') : `00-${instance.since}-${instance.id}`;

      const trainerMap =
        map.get(date) ?? new Map<string, EventInstanceWithEventFragment[]>();
      trainerMap.set(trainers, (trainerMap.get(trainers) ?? []).concat([instance]));
      map.set(date, trainerMap);
    });
    const list = Array.from(map.entries()).map(([date, itemMap]) => ([
      date,
      Array.from(itemMap.entries()).map(([trainers, items]) => {
        items.sort((x, y) => x.since.localeCompare(y.since));
        return [trainers, items] as const;
      }).sort((x, y) => x[0].localeCompare(y[0])),
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

      {dataByDay.map(([date, groups], i) => (
          <React.Fragment key={i}>
            <div className="text-2xl tracking-wide mt-8 mb-2">
              {formatWeekDay(new Date(date))}
            </div>

            <div className="flex justify-start flex-wrap gap-2 ml-2 pl-5 border-l-4 border-accent-10">
              {groups.map(([ids, items]) => {
                const firstEvent = items[0]!.event;
                const withLocation = items.find(x => !!x.event?.location?.name || !!x.event?.locationText);
                const location = withLocation?.event?.location?.name || withLocation?.event?.locationText;
                return (
                  <Card key={ids} className="group min-w-[200px] w-72 pl-1 rounded-lg border-accent-7 border">
                    <div className="ml-3">
                      {firstEvent?.type !== 'LESSON' ? (
                        <div className="text-sm text-accent-11">
                          {formatEventType(firstEvent)}
                        </div>
                      ) : null}
                      {location && firstEvent?.type === 'LESSON' && (
                        <div className="text-sm text-accent-11">
                          {location}
                        </div>
                      )}
                      <div className="text-xl mb-1">
                        {firstEvent?.type !== 'LESSON' ? (
                          <Link href={`/akce/${firstEvent?.id}`}>
                            {firstEvent?.name || firstEvent?.eventTrainersList.map(x => x.name).join(', ')}
                          </Link>
                        ) : (
                          firstEvent?.eventTrainersList.map(x => x.name).join(', ')
                        )}
                      </div>

                      {firstEvent?.type !== 'LESSON' && (
                        <EventSummary instance={items[0]!} />
                      )}
                    </div>
                    {firstEvent?.type === 'LESSON' && (
                      items
                        .sort((x, y) => x.since.localeCompare(y.since))
                        .map((item) => <EventButton key={item.id} instance={item} viewer='trainer' />)
                    )}
                  </Card>
                );
              })}
            </div>
          </React.Fragment>
        ))}
    </div>
  );
}

export default Agenda
