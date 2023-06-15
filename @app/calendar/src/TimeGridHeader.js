import PropTypes from 'prop-types'
import clsx from 'clsx'
import scrollbarSize from 'dom-helpers/scrollbarSize'
import React from 'react'
import DateContentRow from './DateContentRow'
import localizer from './localizer'

class TimeGridHeader extends React.Component {
  handleHeaderClick = (date, view, e) => {
    e.preventDefault()
    this.props.onDrillDown?.(date, view);
  }

  renderHeaderCells(range) {
    let {getDrilldownView} = this.props

    const today = new Date()

    return range.map((date, i) => {
      let drilldownView = getDrilldownView(date)
      let label = localizer.format(date, 'dd eee')

      let header = (
        <span role="columnheader" aria-sort="none">
          {label}
        </span>
      )

      return (
        <div
          key={i}
          className={clsx(
            'rbc-header',
            localizer.isSameDate(date, today) && 'rbc-today'
          )}
        >
          {drilldownView ? (
            <button
              type="button"
              className="rbc-button-link"
              onClick={(e) => this.handleHeaderClick(date, drilldownView, e)}
            >
              {header}
            </button>
          ) : (
            <span>{header}</span>
          )}
        </div>
      )
    })
  }
  renderRow = (resource) => {
    let {
      events,
      selectable,
      range,
      resizable,
    } = this.props

    const resourceId = resource.resourceId
    let eventsToDisplay = resource
      ? events.filter((event) => event.resourceId === resourceId)
      : events

    return (
      <DateContentRow
        isAllDay
        range={range}
        events={eventsToDisplay}
        resourceId={resourceId}
        className="rbc-allday-cell"
        selectable={selectable}
        selected={this.props.selected}
        onSelect={this.props.onSelectEvent}
        onShowMore={this.props.onShowMore}
        onDoubleClick={this.props.onDoubleClickEvent}
        onKeyPress={this.props.onKeyPressEvent}
        onSelectSlot={this.props.onSelectSlot}
        resizable={resizable}
      />
    )
  }

  render() {
    let TimeGutterHeader = null
    let {
      width,
      resources,
      range,
      events,
      selectable,
      scrollRef,
      isOverflowing,
      resizable,
    } = this.props

    let style = {}
    if (isOverflowing) {
      style.marginRight = `${scrollbarSize() - 1}px`
    }

    const groupedEvents = resources.groupEvents(events)

    return (
      <div
        style={style}
        ref={scrollRef}
        className={clsx('rbc-time-header', isOverflowing && 'rbc-overflowing')}
      >
        <div
          className="rbc-label rbc-time-header-gutter"
          style={{ width, minWidth: width, maxWidth: width }}
        >
          {TimeGutterHeader && <TimeGutterHeader />}
        </div>

        {resources.map(([id, resource], idx) => (
          <div className="rbc-time-header-content" key={id || idx}>
            {resource && (
              <div className="rbc-row rbc-row-resource" key={`resource_${idx}`}>
                <div className="rbc-header">
                  {resource.title}
                </div>
              </div>
            )}
            <div
              className={`rbc-row rbc-time-header-cell${
                range.length <= 1 ? ' rbc-time-header-cell-single-day' : ''
              }`}
            >
              {this.renderHeaderCells(range)}
            </div>
            <DateContentRow
              isAllDay
              range={range}
              events={groupedEvents.get(id) || []}
              resourceId={resource && id}
              className="rbc-allday-cell"
              selectable={selectable}
              selected={this.props.selected}
              onSelect={this.props.onSelectEvent}
              onShowMore={this.props.onShowMore}
              onDoubleClick={this.props.onDoubleClickEvent}
              onKeyPress={this.props.onKeyPressEvent}
              onSelectSlot={this.props.onSelectSlot}
              resizable={resizable}
            />
          </div>
        ))}
      </div>
    )
  }
}

TimeGridHeader.propTypes = {
  range: PropTypes.array.isRequired,
  events: PropTypes.array.isRequired,
  resources: PropTypes.object,
  isOverflowing: PropTypes.bool,

  resizable: PropTypes.bool,
  width: PropTypes.number,

  selected: PropTypes.object,
  selectable: PropTypes.oneOf([true, false, 'ignoreEvents']),

  onSelectSlot: PropTypes.func,
  onSelectEvent: PropTypes.func,
  onDoubleClickEvent: PropTypes.func,
  onKeyPressEvent: PropTypes.func,
  onDrillDown: PropTypes.func,
  onShowMore: PropTypes.func,
  getDrilldownView: PropTypes.func.isRequired,
  scrollRef: PropTypes.any,
}

export default TimeGridHeader
