import clsx from 'clsx'
import React from 'react'
import getWidth from 'dom-helpers/width'
import { Navigate, Event, ViewClass } from './utils/constants'
import { eq, add, agendaHeaderFormat, endOf, format, gt, inEventRange, isSameDate, lt, startOf, timeRangeFormat, range } from './localizer'

const Agenda: ViewClass = ({date, events, length = 7, onSelectEvent}) => {
  const dateColRef = React.useRef<HTMLTableCellElement>(null)
  const timeColRef = React.useRef<HTMLTableCellElement>(null)
  const tbodyRef = React.useRef<HTMLTableSectionElement>(null)

  React.useEffect(() => {
    if (!tbodyRef.current) return
    let firstRow = tbodyRef.current?.firstChild
    if (!firstRow) return
    let widths = [getWidth(firstRow.childNodes[0] as any), getWidth(firstRow.childNodes[1] as any)]
    if (dateColRef.current) dateColRef.current.style.width = widths[0] + 'px'
    if (timeColRef.current) timeColRef.current.style.width = widths[1] + 'px'
  })

  const timeRangeLabel = (day: Date, event: Event) => {
    let { start, end } = event

    let label = "Celý den";
    if (!event.allDay) {
        if (eq(start, end)) {
        label = format(start, 'p')
      } else if (isSameDate(start, end)) {
        label = timeRangeFormat(event)
      } else if (isSameDate(day, start)) {
        label = format(start, 'p')
      } else if (isSameDate(day, end)) {
        label = format(end, 'p')
      }
    }

    return (
      <span className={clsx({
        'rbc-continues-prior': gt(day, start, 'day'),
        'rbc-continues-after': lt(day, end, 'day'),
      })}>
        {label}
      </span>
    )
  }

  let end = add(date, length, 'day')
  events = events.filter((event) => inEventRange(event, {start: startOf(date, 'day'), end: endOf(end, 'day')}));
  events.sort((a, b) => +a.start - +b.start);

  const dateRange = range(date, end, 'day');

  if (!events.length) {
    return (
      <div className="rbc-agenda-view">
        <span className="rbc-agenda-empty">
          Žádné události ve zvoleném období
        </span>
      </div>
    );
  }

  return (
    <div className="rbc-agenda-view">
      <table className="rbc-agenda-table">
        <thead>
          <tr>
            <th className="rbc-header" ref={dateColRef}>Datum</th>
            <th className="rbc-header" ref={timeColRef}>Čas</th>
            <th className="rbc-header">Událost</th>
          </tr>
        </thead>
      </table>
      <div className="rbc-agenda-content">
        <table className="rbc-agenda-table">
          <tbody ref={tbodyRef}>
            {dateRange.map((day, dayKey) => (
              events
                .filter((e) => inEventRange(e, {start: startOf(day, 'day'), end: endOf(day, 'day')}))
                .map((event, idx) => (
                  <tr key={dayKey + '_' + idx}>
                 {idx === 0 ? (
                   <td rowSpan={events.length} className="rbc-agenda-date-cell">
                     {format(day, 'ccc MMM dd')}
                   </td>
                 ) : null}

                 <td className="rbc-agenda-time-cell">
                   {timeRangeLabel(day, event)}
                 </td>

                 <td className="rbc-agenda-event-cell" onClick={() => onSelectEvent?.(event)}>
                   {event.title}
                 </td>
               </tr>
              ))
            ))}
          </tbody>
        </table>
      </div>
    </div>
  )
}

Agenda.range = (start: Date, length = 7) => [start, add(start, length, 'day')];

Agenda.navigate = (date: Date, action: Navigate, length = 7) => {
  switch (action) {
    case Navigate.PREVIOUS:
      return add(date, -length, 'day')
    case Navigate.NEXT:
      return add(date, length, 'day')
    default:
      return date
  }
}

Agenda.title = (start: Date, length = 7) => {
  let end = add(start, length, 'day')
  return agendaHeaderFormat({ start, end })
}
Agenda.name = "Agenda";

export default Agenda
