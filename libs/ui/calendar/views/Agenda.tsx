import { add, startOf } from 'date-arithmetic'
import React from 'react'
import { range } from '../localizer'
import { Navigate, ViewClass } from '../types'
import { EventInstanceExtendedFragment } from '@app/graphql/Event'
import { formatWeekDay } from '../../format-date'
import { useAuth } from '../../use-auth'
import { Card } from '../../Card'
import { EventButton } from '../../EventButton'
import { formatEventType } from '../../format-name'

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

          <div className="flex justify-start flex-wrap gap-2 ml-2 pl-5 border-l-4 border-red-400">
            {Object.entries(groups).map(([ids, items]) => (
              <Card key={ids} className="group min-w-[200px] w-72 rounded-lg border-accent-7 border">
                  <div className="ml-3 mb-0.5">
                    <div className="text-sm text-accent-11">
                      {items[0]!.event!.type === 'LESSON' ? items[0]!.event!.locationText : formatEventType(items[0]!.event)}
                    </div>
                    <div className="text-xl">
                      {items[0]!.event!.name || items[0]!.event!.eventTrainersList.map(x => `${x.person!.firstName} ${x.person!.lastName}`).join(', ')}
                    </div>
                  </div>
                  {items.map((item) => <EventButton key={i} instance={item} />)}
                </Card>
            ))}
          </div>
        </React.Fragment>
      ))}
    </div>
  );
}

/* const OldAgenda = ({ range, events }: any) => {
*   const { onSelectEvent } = React.useContext(SelectionContext);
*
*   const eventsPerDay = React.useMemo(() => {
*     const eventsPerDay = new Map<Date, CalendarEvent[]>();
*     range.forEach(day => {
*       const dayRange = { start: startOf(day, 'day'), end: endOf(day, 'day') };
*       const dayEvents = events.filter((e) => inEventRange(e, dayRange));
*       dayEvents.sort(sortEvents);
*       eventsPerDay.set(day, dayEvents);
*     });
*     return Array.from(eventsPerDay.entries());
*   }, [events, range]);
*
*   return (
*     <div>
*       {eventsPerDay.map(([day, events]) => (
*         <div key={+day}>
*           {format(day, 'ccc MMM dd')}
*           {!events.length && (
*             <>Žádné události v tento den</>
*           )}
*           {events.map((event) => (
*             <div key={event.id} onClick={() => onSelectEvent(event)}>
*               <span className="tabular-nums">
*                 {timeRangeLabel(day, event)}
*               </span>
*               {event.title}
*             </div>
*           ))}
*         </div>
*       ))}
*     </div>
*   )
* } */

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
