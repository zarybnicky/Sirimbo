import React from 'react'
import clsx from 'clsx'
import chunk from 'lodash/chunk'
import { View, Navigate } from './utils/constants'
import DateContentRow from './DateContentRow'
import { sortEvents, add, firstVisibleDay, format, isSameDate, lastVisibleDay, neq, range, visibleDays, inEventRange } from './localizer'

class MonthView extends React.Component {
  constructor(...args) {
    this._pendingSelection = []
  }

  render() {
    let { date, className, events } = this.props;
    let { date: currentDate } = this.props

    let weeks = chunk(visibleDays(date), 7);

    return (
      <div
        className={clsx('rbc-month-view', className)}
        role="table"
        aria-label="Month View"
      >
        <div className="rbc-row rbc-month-header" role="row">
          {range(weeks[0][0], weeks[0][weeks[0].length - 1], 'day').map((day, idx) => (
            <div key={'header_' + idx} className="rbc-header">
              <span role="columnheader" aria-sort="none">
                {format(day, 'cccc')}
              </span>
            </div>
          ))}
        </div>

        {weeks.map((week, weekIdx) => (
          <DateContentRow
            key={weekIdx}
            className="rbc-month-row"
            date={date}
            range={week}
            measureRows
            events={events.filter((e) => inEventRange(e, {start: week[0], end: week[week.length - 1]})).sort(sortEvents)}
            selected={this.props.selected}
            renderHeader={({ date, className, ...props }) => (
              <div
                {...props}
                role="cell"
                className={clsx(className, neq(date, currentDate, 'month') && 'rbc-off-range', isSameDate(date, currentDate) && 'rbc-current')}
              >
                <button
                  type="button"
                  className="rbc-button-link"
                  onClick={(e) => {
                    e.preventDefault()
                    clearTimeout(this._selectTimer)
                    this.props.onDrillDown?.(date, View.DAY)
                  }}
                  role="cell"
                >
                  {format(date, 'dd')}
                </button>
              </div>
            )}
            onDrillDown={(date, view) => {
              clearTimeout(this._selectTimer)
              this.props.onDrillDown(date, view)
            }}
            onSelectEvent={(event) => {
              clearTimeout(this._selectTimer)
              this.props.onSelectEvent(event)
            }}
            onSelectSlot={this.handleSelectSlot}
          />
        ))}
      </div>
    );
  }

  handleSelectSlot = (range, slotInfo) => {
    this._pendingSelection = this._pendingSelection.concat(range)

    clearTimeout(this._selectTimer)
    this._selectTimer = setTimeout(() => {
      let slots = this._pendingSelection.slice()
      this._pendingSelection = []
      slots.sort((a, b) => +a - +b)
      const start = new Date(slots[0])
      const end = new Date(slots[slots.length - 1])
      end.setDate(slots[slots.length - 1].getDate() + 1)

      this.props.onSelectSlot?.({
        slots,
        start,
        end,
        action: slotInfo.action,
        bounds: slotInfo.bounds,
        box: slotInfo.box,
      })
    })
  }
}

MonthView.range = (date) => ({ start: firstVisibleDay(date), end: lastVisibleDay(date) })

MonthView.navigate = (date, action) => {
  switch (action) {
    case Navigate.PREVIOUS:
      return add(date, -1, 'month')
    case Navigate.NEXT:
      return add(date, 1, 'month')
    default:
      return date
  }
}

MonthView.title = (date) => format(date, 'MMMM yyyy')
MonthView.name = "Měsíc";

export default MonthView
