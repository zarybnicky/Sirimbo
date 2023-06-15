import PropTypes from 'prop-types'
import React, { useRef, useEffect } from 'react'
import getWidth from 'dom-helpers/width'

import { navigate } from './utils/constants'
import { add, agendaHeaderFormat, endOf, format, gt, inEventRange, isSameDate, lt, startOf } from './localizer'
import clsx from 'clsx'

function Agenda({date, events, length, onDoubleClickEvent, onSelectEvent}) {
  const headerRef = useRef(null)
  const dateColRef = useRef(null)
  const timeColRef = useRef(null)
  const contentRef = useRef(null)
  const tbodyRef = useRef(null)

  useEffect(() => {
    _adjustHeader()
  })

  const timeRangeLabel = (day, event) => {
    let { start, end } = event

    let label = "Celý den";
    if (!event.allDay) {
      if (localizer.eq(start, end)) {
        label = format(start, 'p')
      } else if (isSameDate(start, end)) {
        label = format({ start, end }, 'timeRangeFormat')
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

  const _adjustHeader = () => {
    if (!tbodyRef.current) return

    let firstRow = tbodyRef.current.firstChild

    if (!firstRow) return

    let _widths = []
    let widths = _widths

    _widths = [getWidth(firstRow.children[0]), getWidth(firstRow.children[1])]

    if (widths[0] !== _widths[0] || widths[1] !== _widths[1]) {
      dateColRef.current.style.width = _widths[0] + 'px'
      timeColRef.current.style.width = _widths[1] + 'px'
    }
  }

  let end = add(date, length, 'day')
  events = events.filter((event) => inEventRange({ event, range: {start: startOf(date, 'day'), end: endOf(end, 'day')}}));
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
      <table ref={headerRef} className="rbc-agenda-table">
        <thead>
          <tr>
            <th className="rbc-header" ref={dateColRef}>
              Datum
            </th>
            <th className="rbc-header" ref={timeColRef}>
                  Čas
            </th>
            <th className="rbc-header">
              Událost
            </th>
          </tr>
        </thead>
      </table>
      <div className="rbc-agenda-content" ref={contentRef}>
        <table className="rbc-agenda-table">
          <tbody ref={tbodyRef}>
            {events
             .filter((e) => inRange(e, startOf(day, 'day'), endOf(day, 'day')))
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

                 <td
                   className="rbc-agenda-event-cell"
                   onClick={(e) => onSelectEvent?.(event, e)}
                   onDoubleClick={(e) => onDoubleClickEvent?.(event, e)}
                 >
                   {event.title}
                 </td>
               </tr>
             ))}
          </tbody>
        </table>
      </div>
    </div>
  )
}

Agenda.propTypes = {
  date: PropTypes.instanceOf(Date),
  events: PropTypes.array,
  length: PropTypes.number.isRequired,
  onSelectEvent: PropTypes.func,
  onDoubleClickEvent: PropTypes.func,
  selected: PropTypes.object,
}

Agenda.defaultProps = {
  length: 7,
}

Agenda.range = (start, { length = Agenda.defaultProps.length, }) => {
  let end = add(start, length, 'day')
  return { start, end }
}

Agenda.navigate = (
  date,
  action,
  { length = Agenda.defaultProps.length }
) => {
  switch (action) {
    case navigate.PREVIOUS:
      return add(date, -length, 'day')
    case navigate.NEXT:
      return add(date, length, 'day')
    default:
      return date
  }
}

Agenda.title = (start, { length = Agenda.defaultProps.length }) => {
  let end = add(start, length, 'day')
  return agendaHeaderFormat({ start, end })
}
Agenda.name = "Agenda";

export default Agenda
