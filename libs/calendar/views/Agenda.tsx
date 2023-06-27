import { add, endOf, eq, startOf } from 'date-arithmetic'
import React from 'react'
import { SelectionContext } from '../SelectContext'
import { format, inEventRange, range, shortTimeIntl, sortEvents } from '../localizer'
import { CalendarEvent, Navigate, ViewClass } from '../types'

const timeRangeLabel = (day: Date, { start, end, allDay }: CalendarEvent) => {
  if (allDay) {
    return "Celý den"
  }
  if (eq(start, end)) {
    return shortTimeIntl.format(start)
  }
  if (eq(start, end, 'day')) {
      return shortTimeIntl.formatRange(start, end);
  }
  if (eq(day, start, 'day')) {
    return shortTimeIntl.format(start)
  }
  if (eq(day, end, 'day')) {
    return shortTimeIntl.format(end);
  }
}

const Agenda: ViewClass = ({ range, events }) => {
  const { onSelectEvent } = React.useContext(SelectionContext);

  const eventsPerDay = React.useMemo(() => {
    const eventsPerDay = new Map<Date, CalendarEvent[]>();
    range.forEach(day => {
      const dayRange = { start: startOf(day, 'day'), end: endOf(day, 'day') };
      const dayEvents = events.filter((e) => inEventRange(e, dayRange));
      dayEvents.sort(sortEvents);
      eventsPerDay.set(day, dayEvents);
    });
    return Array.from(eventsPerDay.entries());
  }, [events, range]);

  return (
    <div>
      {eventsPerDay.map(([day, events]) => (
        <div key={+day}>
          {format(day, 'ccc MMM dd')}
          {!events.length && (
            <>Žádné události v tento den</>
          )}
          {events.map((event) => (
            <div key={event.id} onClick={() => onSelectEvent(event)}>
              <span className="tabular-nums">
                {timeRangeLabel(day, event)}
              </span>
              {event.title}
            </div>
          ))}
        </div>
      ))}
    </div>
  )
}

Agenda.range = (start: Date) => range(start, add(start, 7, 'day'), 'day');

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
