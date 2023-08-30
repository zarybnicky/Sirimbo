import { EventInstanceExtendedFragment } from '@app/graphql/Event'
import { Card } from '@app/ui/Card'
import { EventButton } from '@app/ui/EventButton'
import { formatWeekDay } from '@app/ui/format'
import { formatEventType } from '@app/ui/format'
import { useAuth } from '@app/ui/use-auth'
import { add, startOf } from 'date-arithmetic'
import React from 'react'
import { range } from '../localizer'
import { Navigate, ViewClass } from '../types'
import Link from 'next/link'

const Agenda: ViewClass = ({ events }) => {
  const { user } = useAuth()
  const dataByDay = React.useMemo(() => {
    const obj: { [date: string]: { [trainers: string]: EventInstanceExtendedFragment[]; } } = {};
    events.forEach((item) => {
      const day = startOf(new Date(item.since), 'day').toString();
      const trainers = item.event!.type === 'LESSON' ? item.event!.eventTrainersList.map(x => x.person!.id).join(',') : `i${item.id}`;
      obj[day] = obj[day] || {};
      obj[day]![trainers] = obj[day]![trainers] || [];
      obj[day]![trainers]!.push(item);
    });
    return obj;
  }, [events]);

  return (
    <div className={user ? 'col-full-width p-4 lg:pb-8' : 'col-feature min-h-[60vh] py-4 mb-8'}>
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
            {Object.entries(groups).map(([ids, items]) => (
              <Card key={ids} className="group min-w-[200px] w-72 rounded-lg border-accent-7 border">
                <div className="ml-3 mb-0.5">
                  {items[0]!.event!.type !== 'LESSON' ? (
                    <div className="text-sm text-accent-11">
                      {formatEventType(items[0]!.event)}
                    </div>
                  ) : null}
                  {items[0]!.event!.locationText && (
                    <div className="text-sm text-accent-11">
                      {items[0]!.event!.locationText}
                    </div>
                  )}
                  <div className="text-xl">
                    {items[0]!.event!.type !== 'LESSON' ? (
                      <Link href={`/akce/${items[0]!.event!.id}`}>
                        {items[0]!.event!.name || items[0]!.event!.eventTrainersList.map(x => x.person?.name).join(', ')}
                      </Link>
                    ) : (
                      items[0]!.event!.name || items[0]!.event!.eventTrainersList.map(x => x.person?.name).join(', ')
                    )}
                  </div>
                </div>
                {items[0]!.event!.type === 'LESSON' ? (
                  items.map((item) => <EventButton key={i} instance={item} />)
                ) : null}
              </Card>
            ))}
          </div>
        </React.Fragment>
      ))}
    </div>
  );
}

Agenda.range = (start: Date) => range(start, add(start, 6, 'day'), 'day');

Agenda.navigate = (date: Date, action: Navigate) => {
  switch (action) {
    case Navigate.PREVIOUS:
      return add(date, -7, 'day')
    case Navigate.NEXT:
      return add(date, 7, 'day')
    default:
      return date
  }
}

export default Agenda
