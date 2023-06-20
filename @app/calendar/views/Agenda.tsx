import clsx from 'clsx'
import React, { useContext } from 'react'
import getWidth from 'dom-helpers/width'
import { Navigate, CalendarEvent, ViewClass } from '../types'
import { eq, add, endOf, format, gt, inEventRange, lt, startOf, range } from '../localizer'
import { useLayoutEffect } from '@radix-ui/react-use-layout-effect';
import { SelectionContext } from '../SelectContext'

const Agenda: ViewClass = ({ date, range, events }) => {
  const dateColRef = React.useRef<HTMLTableCellElement>(null)
  const timeColRef = React.useRef<HTMLTableCellElement>(null)
  const tbodyRef = React.useRef<HTMLTableSectionElement>(null)
  const { onSelectEvent } = useContext(SelectionContext);

  useLayoutEffect(() => {
    let firstRow = tbodyRef.current?.firstChild
    if (!firstRow) return
    if (dateColRef.current) {
      dateColRef.current.style.width = getWidth(firstRow.childNodes[0] as any) + 'px'
    }
    if (timeColRef.current) {
      timeColRef.current.style.width = getWidth(firstRow.childNodes[1] as any) + 'px'
    }
  })

  const timeRangeLabel = (day: Date, event: CalendarEvent) => {
    let { start, end } = event

    let label = "";
    if (event.allDay) {
      label = "Celý den"
    } else if (eq(start, end)) {
      label = format(start, 'p')
    } else if (eq(start, end, 'day')) {
      label = `${format(start, 'p')} – ${format(end, 'p')}`;
    } else if (eq(day, start, 'day')) {
      label = format(start, 'p')
    } else if (eq(day, end, 'day')) {
      label = format(end, 'p')
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

  let end = add(date, 7, 'day')
  events = events.filter((event) => inEventRange(event, {start: startOf(date, 'day'), end: endOf(end, 'day')}));
  events.sort((a, b) => +a.start - +b.start);

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
            {range.map((day, dayKey) => (
              events
                .filter((e) => inEventRange(e, {start: startOf(day, 'day'), end: endOf(day, 'day')}))
                .map((event, idx, daysEvents) => (
                  <tr key={dayKey + '_' + idx}>
                    {idx === 0 ? (
                      <td rowSpan={daysEvents.length} className="rbc-agenda-date-cell">
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
