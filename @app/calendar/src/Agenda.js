import PropTypes from 'prop-types'
import React, { useRef, useEffect } from 'react'
import addClass from 'dom-helpers/addClass'
import removeClass from 'dom-helpers/removeClass'
import getWidth from 'dom-helpers/width'
import scrollbarSize from 'dom-helpers/scrollbarSize'

import { navigate } from './utils/constants'
import { inRange } from './utils/eventLevels'
import localizer, { messages } from './localizer'

function Agenda({
  date,
  events,
  length,
  onDoubleClickEvent,
  onSelectEvent,
}) {
  const headerRef = useRef(null)
  const dateColRef = useRef(null)
  const timeColRef = useRef(null)
  const contentRef = useRef(null)
  const tbodyRef = useRef(null)

  useEffect(() => {
    _adjustHeader()
  })

  const renderDay = (day, events, dayKey) => {
    events = events.filter((e) => inRange(e, localizer.startOf(day, 'day'), localizer.endOf(day, 'day')))

    return events.map((event, idx) => (
      <tr key={dayKey + '_' + idx}>
        {idx === 0 ? (
          <td rowSpan={events.length} className="rbc-agenda-date-cell">
            {localizer.format(day, 'ccc MMM dd')}
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
    ))
  }

  const timeRangeLabel = (day, event) => {
    let { start, end } = event

    let label = messages.allDay;
    if (!event.allDay) {
      if (localizer.eq(start, end)) {
        label = localizer.format(start, 'p')
      } else if (localizer.isSameDate(start, end)) {
        label = localizer.format({ start, end }, 'timeRangeFormat')
      } else if (localizer.isSameDate(day, start)) {
        label = localizer.format(start, 'p')
      } else if (localizer.isSameDate(day, end)) {
        label = localizer.format(end, 'p')
      }
    }

    let labelClass = '';
    if (localizer.gt(day, start, 'day')) labelClass = 'rbc-continues-prior'
    if (localizer.lt(day, end, 'day')) labelClass += ' rbc-continues-after'

    return (
      <span className={labelClass.trim()}>
        {label}
      </span>
    )
  }

  const _adjustHeader = () => {
    if (!tbodyRef.current) return

    let header = headerRef.current
    let firstRow = tbodyRef.current.firstChild

    if (!firstRow) return

    let isOverflowing =
      contentRef.current.scrollHeight > contentRef.current.clientHeight

    let _widths = []
    let widths = _widths

    _widths = [getWidth(firstRow.children[0]), getWidth(firstRow.children[1])]

    if (widths[0] !== _widths[0] || widths[1] !== _widths[1]) {
      dateColRef.current.style.width = _widths[0] + 'px'
      timeColRef.current.style.width = _widths[1] + 'px'
    }

    if (isOverflowing) {
      addClass(header, 'rbc-header-overflowing')
      header.style.marginRight = scrollbarSize() + 'px'
    } else {
      removeClass(header, 'rbc-header-overflowing')
    }
  }

  let end = localizer.add(date, length, 'day')
  let range = localizer.range(date, end, 'day')

  events = events.filter((event) => inRange(event, localizer.startOf(date, 'day'), localizer.endOf(end, 'day')))

  events.sort((a, b) => +a.start - +b.start)

  return (
    <div className="rbc-agenda-view">
      {events.length !== 0 ? (
        <React.Fragment>
          <table ref={headerRef} className="rbc-agenda-table">
            <thead>
              <tr>
                <th className="rbc-header" ref={dateColRef}>
                  {messages.date}
                </th>
                <th className="rbc-header" ref={timeColRef}>
                  {messages.time}
                </th>
                <th className="rbc-header">{messages.event}</th>
              </tr>
            </thead>
          </table>
          <div className="rbc-agenda-content" ref={contentRef}>
            <table className="rbc-agenda-table">
              <tbody ref={tbodyRef}>
                {range.map((day, idx) => renderDay(day, events, idx))}
              </tbody>
            </table>
          </div>
        </React.Fragment>
      ) : (
        <span className="rbc-agenda-empty">{messages.noEventsInRange}</span>
      )}
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
  length: 30,
}

Agenda.range = (start, { length = Agenda.defaultProps.length, }) => {
  let end = localizer.add(start, length, 'day')
  return { start, end }
}

Agenda.navigate = (
  date,
  action,
  { length = Agenda.defaultProps.length }
) => {
  switch (action) {
    case navigate.PREVIOUS:
      return localizer.add(date, -length, 'day')
    case navigate.NEXT:
      return localizer.add(date, length, 'day')
    default:
      return date
  }
}

Agenda.title = (start, { length = Agenda.defaultProps.length }) => {
  let end = localizer.add(start, length, 'day')
  return localizer.format({ start, end }, 'agendaHeaderFormat')
}

export default Agenda
