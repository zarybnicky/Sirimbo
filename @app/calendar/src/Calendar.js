import PropTypes from 'prop-types'
import React from 'react'
import { uncontrollable } from 'uncontrollable'
import clsx from 'clsx'

import { navigate, views } from './utils/constants'
import VIEWS from './Views'
import Toolbar from './Toolbar'

class Calendar extends React.Component {
  static propTypes = {
    view: PropTypes.string,
    defaultView: PropTypes.string,
    events: PropTypes.arrayOf(PropTypes.object),
    backgroundEvents: PropTypes.arrayOf(PropTypes.object),
    resources: PropTypes.arrayOf(PropTypes.object),
    onNavigate: PropTypes.func,
    onView: PropTypes.func,
    onDrillDown: PropTypes.func,
    onRangeChange: PropTypes.func,
    onSelectSlot: PropTypes.func,
    onSelectEvent: PropTypes.func,
    onDoubleClickEvent: PropTypes.func,
    onKeyPressEvent: PropTypes.func,
    onSelecting: PropTypes.func,
    onShowMore: PropTypes.func,
    selected: PropTypes.object,
    doShowMoreDrillDown: PropTypes.bool,
    drilldownView: PropTypes.string,
    getDrilldownView: PropTypes.func,
    length: PropTypes.number,
    step: PropTypes.number,
    timeslots: PropTypes.number,
    showMultiDayTimes: PropTypes.bool,
    min: PropTypes.instanceOf(Date),
    max: PropTypes.instanceOf(Date),
    scrollToTime: PropTypes.instanceOf(Date),
    dayLayoutAlgorithm: PropTypes.oneOf(['overlap', 'no-overlap']),
  }

  static defaultProps = {
    events: [],
    backgroundEvents: [],
    popup: false,
    view: views.MONTH,
    step: 30,
    length: 30,

    doShowMoreDrillDown: true,
    drilldownView: views.DAY,
    dayLayoutAlgorithm: 'overlap',
  }

  getDrilldownView = (date) => {
    const { view, drilldownView, getDrilldownView } = this.props
    if (!getDrilldownView) return drilldownView
    return getDrilldownView(date, view, views);
  }

  render() {
    let {
      view,
      events,
      backgroundEvents,
      style,
      className,
      date: current,
      length,
      showMultiDayTimes,
      onShowMore,
      doShowMoreDrillDown,
      ...props
    } = this.props

    current = current || new Date()
    const View = VIEWS[this.props.view];

    return (
      <div className={clsx(className, 'rbc-calendar')} style={style}>
        <Toolbar
          date={current}
          view={view}
          label={View.title(current, { length })}
          onView={this.handleViewChange}
          onNavigate={this.handleNavigate}
        />
        <View
          {...props}
          events={events}
          backgroundEvents={backgroundEvents}
          date={current}
          length={length}
          showMultiDayTimes={showMultiDayTimes}
          getDrilldownView={this.getDrilldownView}
          onNavigate={this.handleNavigate}
          onDrillDown={this.handleDrillDown}
          onSelectEvent={this.handleSelectEvent}
          onDoubleClickEvent={this.handleDoubleClickEvent}
          onKeyPressEvent={this.handleKeyPressEvent}
          onSelectSlot={this.handleSelectSlot}
          onShowMore={onShowMore}
          doShowMoreDrillDown={doShowMoreDrillDown}
        />
      </div>
    )
  }

  handleRangeChange = (date, viewComponent, view) => {
    let { onRangeChange } = this.props

    if (onRangeChange) {
      if (viewComponent.range) {
        onRangeChange(viewComponent.range(date), view)
      } else {
        if (process.env.NODE_ENV !== 'production') {
          console.error('onRangeChange prop not supported for this view')
        }
      }
    }
  }

  handleNavigate = (action, newDate) => {
    let { view, date, onNavigate, ...props } = this.props
    const View = VIEWS[this.props.view]

    newDate = newDate || date || new Date()
    date =
      action === navigate.TODAY ? (today || new Date())
      : action === navigate.DATE ? newDate
      : View.navigate(newDate, action, props)

    onNavigate(date, view, action)
    this.handleRangeChange(date, View)
  }

  handleViewChange = (view) => {
    if (view !== this.props.view && views.includes(view)) {
      this.props.onView?.(view)
    }
    this.handleRangeChange(this.props.date || new Date(), VIEWS[view], view)
  }

  handleSelectEvent = (...args) => this.props.onSelectEvent?.(...args)
  handleDoubleClickEvent = (...args) => this.props.onDoubleClickEvent?.(...args)
  handleKeyPressEvent = (...args) => this.props.onKeyPressEvent?.(args)
  handleSelectSlot = (slotInfo) => this.props.onSelectSlot?.(slotInfo)
  handleDrillDown = (date, view) => {
    const { onDrillDown } = this.props
    if (onDrillDown) {
      onDrillDown(date, view, this.drilldownView)
      return
    }
    if (view) this.handleViewChange(view)

    this.handleNavigate(navigate.DATE, date)
  }
}

export default uncontrollable(Calendar, {
  view: 'onView',
  date: 'onNavigate',
  selected: 'onSelectEvent',
})
