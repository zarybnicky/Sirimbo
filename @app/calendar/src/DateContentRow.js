import React, { createRef } from 'react'
import clsx from 'clsx'
import getHeight from 'dom-helpers/height'
import PropTypes from 'prop-types'

import BackgroundCells from './BackgroundCells'
import EventRow from './EventRow'
import EventEndingRow from './EventEndingRow'
import { getSlotMetrics } from './utils/DateSlotMetrics'
import WeekWrapper from './addons/dragAndDrop/WeekWrapper'
import { isSameDate } from './localizer'

class DateContentRow extends React.Component {
  constructor(...args) {
    super(...args)

    this.containerRef = createRef()
    this.headingRowRef = createRef()
    this.eventRowRef = createRef()

    this.slotMetrics = getSlotMetrics()
  }

  handleSelectSlot = (slot) => {
    const { range, onSelectSlot } = this.props

    onSelectSlot(range.slice(slot.start, slot.end + 1), slot)
  }

  handleShowMore = (slot, target) => {
    const { range, onShowMore } = this.props
    let metrics = this.slotMetrics(this.props)
    let row = this.containerRef.current?.querySelector('.rbc-row-bg');

    let cell
    if (row) cell = row.children[slot - 1]

    let events = metrics.getEventsForSlot(slot)
    onShowMore(events, range[slot - 1], cell, slot, target)
  }

  getContainer = () => {
    const { container } = this.props
    return container ? container() : this.containerRef.current
  }

  getRowLimit() {
    const eventHeight = getHeight(this.eventRowRef.current)
    const headingHeight = this.headingRowRef?.current ? getHeight(this.headingRowRef.current) : 0
    const eventSpace = getHeight(this.containerRef.current) - headingHeight
    return Math.max(Math.floor(eventSpace / eventHeight), 1)
  }

  renderHeadingCell = (date, index) => {
    let { renderHeader } = this.props

    return renderHeader({
      date,
      key: `header_${index}`,
      className: clsx('rbc-date-cell', isSameDate(date, new Date()) && 'rbc-now'),
    })
  }

  renderDummy = () => {
    let { className, range, renderHeader } = this.props
    return (
      <div className={className} ref={this.containerRef}>
        <div className='rbc-row-content'>
          {renderHeader && (
            <div className="rbc-row" ref={this.headingRowRef}>
              {range.map(this.renderHeadingCell)}
            </div>
          )}
          <div className="rbc-row" ref={this.eventRowRef}>
            <div className="rbc-row-segment">
              <div className="rbc-event">
                <div className="rbc-event-content">&nbsp;</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    )
  }

  render() {
    const {
      date,
      range,
      className,
      selected,
      renderForMeasure,
      renderHeader,
      onSelect,
      onSelectStart,
      onSelectEnd,
      onDoubleClick,
      onKeyPress,
      resourceId,
      isAllDay,
      resizable,
    } = this.props

    if (renderForMeasure) return this.renderDummy()

    let metrics = this.slotMetrics(this.props)
    let { levels, extra } = metrics

    const eventRowProps = {
      selected,
      onSelect,
      onDoubleClick,
      onKeyPress,
      resourceId,
      slotMetrics: metrics,
      resizable,
    }

    return (
      <div className={className} role="rowgroup" ref={this.containerRef}>
        <BackgroundCells
          date={date}
          range={range}
          container={this.getContainer}
          onSelectStart={onSelectStart}
          onSelectEnd={onSelectEnd}
          onSelectSlot={this.handleSelectSlot}
          resourceId={resourceId}
        />

        <div className='rbc-row-content' role="row">
          {renderHeader && (
            <div className="rbc-row " ref={this.headingRowRef}>
              {range.map(this.renderHeadingCell)}
            </div>
          )}
          <WeekWrapper isAllDay={isAllDay} {...eventRowProps}>
            {levels.map((segs, idx) => (
              <EventRow key={idx} segments={segs} {...eventRowProps} />
            ))}
            {!!extra.length && (
              <EventEndingRow segments={extra} onShowMore={this.handleShowMore} {...eventRowProps} />
            )}
          </WeekWrapper>
        </div>
      </div>
    )
  }
}

DateContentRow.propTypes = {
  date: PropTypes.instanceOf(Date),
  events: PropTypes.array.isRequired,
  range: PropTypes.array.isRequired,

  resizable: PropTypes.bool,
  resourceId: PropTypes.any,
  renderForMeasure: PropTypes.bool,
  renderHeader: PropTypes.func,

  container: PropTypes.func,
  selected: PropTypes.object,

  onShowMore: PropTypes.func,
  onSelectSlot: PropTypes.func,
  onSelect: PropTypes.func,
  onSelectEnd: PropTypes.func,
  onSelectStart: PropTypes.func,
  onDoubleClick: PropTypes.func,
  onKeyPress: PropTypes.func,

  isAllDay: PropTypes.bool,
}

export default DateContentRow
