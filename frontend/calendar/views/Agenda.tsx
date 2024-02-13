import { EventInstanceWithEventFragment } from '@app/graphql/Event'
import { Card } from '@app/ui/Card'
import { EventButton } from '@app/ui/EventButton'
import { formatWeekDay } from '@app/ui/format'
import { formatEventType } from '@app/ui/format'
import { startOf } from 'date-arithmetic'
import React from 'react'
import { ViewClass } from '../types'
import Link from 'next/link'
import { EventSummary } from '@/ui/EventSummary'
import { UpsertEventSmallButton } from '@/ui/event-form/UpsertEventForm'
import { DeleteInstanceButton } from '@/ui/DeleteEventButton'

const Agenda: ViewClass = ({ events }) => {
  const dataByDay = React.useMemo(() => {
    const obj: { [date: string]: { [trainers: string]: EventInstanceWithEventFragment[]; } } = {};
    events.forEach((item) => {
      const day = startOf(new Date(item.since), 'day').toString();
      const event = item.event;
      const trainers = !event ? '' : ['LESSON'].includes(event.type) ? item.event!.eventTrainersList.map(x => x.person!.id).join(',') : `i${item.id}`;
      obj[day] = obj[day] || {};
      obj[day]![trainers] = obj[day]![trainers] || [];
      obj[day]![trainers]!.push(item);
    });
    return obj;
  }, [events]);

  return (
    <div className="col-full-width p-4 lg:pb-8 overflow-y-auto overscroll-contain">
      {!events?.length && (
        <div className="border border-accent-6 p-2 bg-accent-1 text-accent-12 rounded-md">
          Žádné tréninky pro tento týden
        </div>
      )}

      {Object.entries(dataByDay).map(([date, groups], i) => (
        <React.Fragment key={i}>
          <div className="text-2xl tracking-wide mt-8 mb-2">
            {formatWeekDay(new Date(date))}
          </div>

          <div className="flex justify-start flex-wrap gap-2 ml-2 pl-5 border-l-4 border-accent-10">
            {Object.entries(groups).map(([ids, items]) => {
              const firstEvent = items[0]!.event;
              const withLocation = items.find(x => !!x.event?.location?.name || !!x.event?.locationText);
              const location = withLocation?.event?.location?.name || withLocation?.event?.locationText;
              return (
                <Card key={ids} className="group min-w-[200px] w-72 rounded-lg border-accent-7 border">
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
                          {firstEvent?.name || firstEvent?.eventTrainersList.map(x => x.person?.name).join(', ')}
                        </Link>
                      ) : (
                        firstEvent?.eventTrainersList.map(x => x.person?.name).join(', ')
                      )}
                    </div>

                    {firstEvent?.type !== 'LESSON' && (
                      <>
                        <EventSummary instance={items[0]!} />
                        {items[0]!.event && <UpsertEventSmallButton className="absolute top-4 right-10" event={items[0]!.event} />}
                        {items[0] && <DeleteInstanceButton className="absolute top-4 right-4" instance={items[0]} />}
                      </>
                    )}
                  </div>
                  {firstEvent?.type === 'LESSON' && (
                    items.map((item) => <EventButton key={item.id} instance={item} viewer='trainer' />)
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
