import React, { createRef } from 'react'
import PropTypes from 'prop-types'
import clsx from 'clsx'

import chunk from 'lodash/chunk'

import { navigate, views } from './utils/constants'
import getPosition from 'dom-helpers/position'
import * as animationFrame from 'dom-helpers/animationFrame'

import PopOverlay from './PopOverlay'
import DateContentRow from './DateContentRow'

import { inRange } from './utils/eventLevels'
import { sortEvents, add, firstVisibleDay, format, isSameDate, lastVisibleDay, neq, range, visibleDays } from './localizer'

let eventsForWeek = (evts, start, end) => evts.filter((e) => inRange(e, start, end))

class MonthView extends React.Component {
  constructor(...args) {
    super(...args)

    this.state = {
      rowLimit: 5,
      needLimitMeasure: true,
      date: null,
    }
    this.containerRef = createRef()
    this.slotRowRef = createRef()

    this._bgRows = []
    this._pendingSelection = []
  }

  static getDerivedStateFromProps({ date }, state) {
    return {
      date,
      needLimitMeasure: neq(date, state.date, 'month'),
    }
  }

  componentDidMount() {
    let running

    if (this.state.needLimitMeasure) this.measureRowLimit(this.props)

    window.addEventListener(
      'resize',
      (this._resizeListener = () => {
        if (!running) {
          animationFrame.request(() => {
            running = false
            this.setState({ needLimitMeasure: true }) //eslint-disable-line
          })
        }
      }),
      false
    )
  }

  componentDidUpdate() {
    if (this.state.needLimitMeasure) this.measureRowLimit(this.props)
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this._resizeListener, false)
  }

  getContainer = () => {
    return this.containerRef.current
  }

  render() {
    let { date, className } = this.props,
      month = visibleDays(date),
      weeks = chunk(month, 7)

    this._weekCount = weeks.length

    return (
      <div
        className={clsx('rbc-month-view', className)}
        role="table"
        aria-label="Month View"
        ref={this.containerRef}
      >
        <div className="rbc-row rbc-month-header" role="row">
          {this.renderHeaders(weeks[0])}
        </div>
        {weeks.map(this.renderWeek)}
        {this.props.popup && this.renderOverlay()}
      </div>
    )
  }

  renderWeek = (week, weekIdx) => {
    let {events, selected, date} = this.props
    const { needLimitMeasure } = this.state

    const weeksEvents = eventsForWeek([...events], week[0], week[week.length - 1])
    weeksEvents.sort((a, b) => sortEvents(a, b))

    return (
      <DateContentRow
        key={weekIdx}
        ref={weekIdx === 0 ? this.slotRowRef : undefined}
        container={this.getContainer}
        className="rbc-month-row"
        date={date}
        range={week}
        events={weeksEvents}
        selected={selected}
        renderHeader={this.readerDateHeading}
        renderForMeasure={needLimitMeasure}
        onShowMore={this.handleShowMore}
        onSelect={this.handleSelectEvent}
        onDoubleClick={this.handleDoubleClickEvent}
        onKeyPress={this.handleKeyPressEvent}
        onSelectSlot={this.handleSelectSlot}
        resizable={this.props.resizable}
      />
    )
  }

  readerDateHeading = ({ date, className, ...props }) => {
    let { date: currentDate, getDrilldownView } = this.props
    let isOffRange = neq(date, currentDate, 'month')
    let isCurrent = isSameDate(date, currentDate)
    let drilldownView = getDrilldownView(date)
    let label = format(date, 'dd')

    return (
      <div
        {...props}
        className={clsx(
          className,
          isOffRange && 'rbc-off-range',
          isCurrent && 'rbc-current'
        )}
        role="cell"
      >
        {drilldownView ? (
          <button
            type="button"
            className="rbc-button-link"
            onClick={(e) => this.handleHeadingClick(date, drilldownView, e)}
            role="cell"
          >
            {label}
          </button>
        ) : (
          <span>{label}</span>
        )}
      </div>
    )
  }

  renderHeaders(row) {
    let first = row[0]
    let last = row[row.length - 1]

    return range(first, last, 'day').map((day, idx) => (
      <div key={'header_' + idx} className="rbc-header">
        <span role="columnheader" aria-sort="none">
          {format(day, 'cccc')}
        </span>
      </div>
    ))
  }

  renderOverlay() {
    let overlay = this.state?.overlay ?? {}
    let {selected, handleDragStart} = this.props

    const onHide = () => this.setState({ overlay: null })

    return (
      <PopOverlay
        overlay={overlay}
        selected={selected}
        ref={this.containerRef}
        handleKeyPressEvent={this.handleKeyPressEvent}
        handleSelectEvent={this.handleSelectEvent}
        handleDoubleClickEvent={this.handleDoubleClickEvent}
        handleDragStart={handleDragStart}
        show={!!overlay.position}
        overlayDisplay={this.overlayDisplay}
        onHide={onHide}
      />
    )
  }

  measureRowLimit() {
    this.setState({
      needLimitMeasure: false,
      rowLimit: this.slotRowRef.current.getRowLimit(),
    })
  }

  handleSelectSlot = (range, slotInfo) => {
    this._pendingSelection = this._pendingSelection.concat(range)

    clearTimeout(this._selectTimer)
    this._selectTimer = setTimeout(() => this.selectDates(slotInfo))
  }

  handleHeadingClick = (date, view, e) => {
    e.preventDefault()
    this.clearSelection()
    this.props.onDrillDown?.(date, view)
  }

  handleSelectEvent = (...args) => {
    this.clearSelection()
    this.props.onSelectEvent?.(...args)
  }

  handleDoubleClickEvent = (...args) => {
    this.clearSelection()
    this.props.onDoubleClickEvent?.(...args)
  }

  handleKeyPressEvent = (...args) => {
    this.clearSelection()
    this.props.onKeyPressEvent?.(...args)
  }

  handleShowMore = (events, date, cell, slot, target) => {
    const {
      popup,
      onDrillDown,
      onShowMore,
      getDrilldownView,
      doShowMoreDrillDown,
    } = this.props
    //cancel any pending selections so only the event click goes through.
    this.clearSelection()

    if (popup) {
      let position = getPosition(cell, this.containerRef.current)

      this.setState({
        overlay: { date, events, position, target },
      })
    } else if (doShowMoreDrillDown) {
      onDrillDown?.(date, getDrilldownView(date) || views.DAY)
    }

    onShowMore?.(events, date, slot)
  }

  overlayDisplay = () => {
    this.setState({
      overlay: null,
    })
  }

  selectDates(slotInfo) {
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
  }

  clearSelection() {
    clearTimeout(this._selectTimer)
    this._pendingSelection = []
  }
}

MonthView.propTypes = {
  events: PropTypes.array.isRequired,
  date: PropTypes.instanceOf(Date),

  min: PropTypes.instanceOf(Date),
  max: PropTypes.instanceOf(Date),

  step: PropTypes.number,

  scrollToTime: PropTypes.instanceOf(Date),
  resizable: PropTypes.bool,
  width: PropTypes.number,
  selected: PropTypes.object,

  onNavigate: PropTypes.func,
  onSelectSlot: PropTypes.func,
  onSelectEvent: PropTypes.func,
  onDoubleClickEvent: PropTypes.func,
  onKeyPressEvent: PropTypes.func,
  onShowMore: PropTypes.func,
  doShowMoreDrillDown: PropTypes.bool,
  onDrillDown: PropTypes.func,
  getDrilldownView: PropTypes.func.isRequired,

  popup: PropTypes.bool,
  handleDragStart: PropTypes.func,
}

MonthView.range = (date) => {
  let start = firstVisibleDay(date)
  let end = lastVisibleDay(date)
  return { start, end }
}

MonthView.navigate = (date, action) => {
  switch (action) {
    case navigate.PREVIOUS:
      return add(date, -1, 'month')

    case navigate.NEXT:
      return add(date, 1, 'month')

    default:
      return date
  }
}

MonthView.title = (date) => format(date, 'MMMM yyyy')
MonthView.name = "Měsíc";

export default MonthView
