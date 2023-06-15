import PropTypes from 'prop-types'
import React from 'react'
import { uncontrollable } from 'uncontrollable'
import clsx from 'clsx'

import { navigate, views } from './utils/constants'
import VIEWS from './Views'
import Toolbar from './Toolbar'

class Calendar extends React.Component {
  static propTypes = {
    /**
     * The current view of the calendar.
     *
     * @default 'month'
     * @controllable onView
     */
    view: PropTypes.string,

    /**
     * The initial view set for the Calendar.
     * @type Calendar.Views ('month'|'week'|'work_week'|'day'|'agenda')
     * @default 'month'
     */
    defaultView: PropTypes.string,

    /**
     * An array of event objects to display on the calendar. Events objects
     * can be any shape, as long as the Calendar knows how to retrieve the
     * following details of the event:
     *
     *  - start time
     *  - end time
     *  - title
     *  - whether its an "all day" event or not
     *  - any resource the event may be related to
     *
     * ```js
     * Event {
     *   title: string,
     *   start: Date,
     *   end: Date,
     *   allDay?: boolean
     *   resource?: any,
     * }
     * ```
     */
    events: PropTypes.arrayOf(PropTypes.object),

    /**
     * An array of background event objects to display on the calendar. Background
     * Events behave similarly to Events but are not factored into Event overlap logic,
     * allowing them to sit behind any Events that may occur during the same period.
     * Background Events objects can be any shape, as long as the Calendar knows how to
     * retrieve the following details of the event:
     *
     *  - start time
     *  - end time
     *
     * ```js
     * BackgroundEvent {
     *   start: Date,
     *   end: Date,
     * }
     * ```
     */
    backgroundEvents: PropTypes.arrayOf(PropTypes.object),
    resources: PropTypes.arrayOf(PropTypes.object),

    /**
     * Callback fired when the `date` value changes.
     *
     * @controllable date
     */
    onNavigate: PropTypes.func,

    /**
     * Callback fired when the `view` value changes.
     *
     * @controllable view
     */
    onView: PropTypes.func,

    /**
     * Callback fired when date header, or the truncated events links are clicked
     *
     */
    onDrillDown: PropTypes.func,

    /**
     *
     * ```js
     * (dates: Date[] | { start: Date; end: Date }, view: 'month'|'week'|'work_week'|'day'|'agenda'|undefined) => void
     * ```
     *
     * Callback fired when the visible date range changes. Returns an Array of dates
     * or an object with start and end dates for BUILTIN views. Optionally new `view`
     * will be returned when callback called after view change.
     *
     * Custom views may return something different.
     */
    onRangeChange: PropTypes.func,

    /**
     * A callback fired when a date selection is made. Only fires when `selectable` is `true`.
     *
     * ```js
     * (
     *   slotInfo: {
     *     start: Date,
     *     end: Date,
     *     resourceId:  (number|string),
     *     slots: Array<Date>,
     *     action: "select" | "click" | "doubleClick",
     *     bounds: ?{ // For "select" action
     *       x: number,
     *       y: number,
     *       top: number,
     *       right: number,
     *       left: number,
     *       bottom: number,
     *     },
     *     box: ?{ // For "click" or "doubleClick" actions
     *       clientX: number,
     *       clientY: number,
     *       x: number,
     *       y: number,
     *     },
     *   }
     * ) => any
     * ```
     */
    onSelectSlot: PropTypes.func,

    /**
     * Callback fired when a calendar event is selected.
     *
     * ```js
     * (event: Object, e: SyntheticEvent) => any
     * ```
     *
     * @controllable selected
     */
    onSelectEvent: PropTypes.func,

    /**
     * Callback fired when a calendar event is clicked twice.
     *
     * ```js
     * (event: Object, e: SyntheticEvent) => void
     * ```
     */
    onDoubleClickEvent: PropTypes.func,

    /**
     * Callback fired when a focused calendar event receives a key press.
     *
     * ```js
     * (event: Object, e: SyntheticEvent) => void
     * ```
     */
    onKeyPressEvent: PropTypes.func,

    /**
     * Callback fired when dragging a selection in the Time views.
     *
     * Returning `false` from the handler will prevent a selection.
     *
     * ```js
     * (range: { start: Date, end: Date, resourceId: (number|string) }) => ?boolean
     * ```
     */
    onSelecting: PropTypes.func,

    /**
     * Callback fired when a +{count} more is clicked
     *
     * ```js
     * (events: Object, date: Date) => any
     * ```
     */
    onShowMore: PropTypes.func,

    /**
     * The selected event, if any.
     */
    selected: PropTypes.object,

    /**
     * Determines whether the drill down should occur when clicking on the "+_x_ more" link.
     * If `popup` is false, and `doShowMoreDrillDown` is true, the drill down will occur as usual.
     * If `popup` is false, and `doShowMoreDrillDown` is false, the drill down will not occur and the `onShowMore` function will trigger.
     */
    doShowMoreDrillDown: PropTypes.bool,

    /**
     * The string name of the destination view for drill-down actions, such
     * as clicking a date header, or the truncated events links. If
     * `getDrilldownView` is also specified it will be used instead.
     *
     * Set to `null` to disable drill-down actions.
     *
     * ```js
     * <Calendar
     *   drilldownView="agenda"
     * />
     * ```
     */
    drilldownView: PropTypes.string,

    /**
     * Functionally equivalent to `drilldownView`, but accepts a function
     * that can return a view name. It's useful for customizing the drill-down
     * actions depending on the target date and triggering view.
     *
     * Return `null` to disable drill-down actions.
     *
     * ```js
     * <Calendar
     *   getDrilldownView={(targetDate, currentViewName) =>
     *     if (currentViewName === 'month')
     *       return 'week'
     *
     *     return null;
     *   }}
     * />
     * ```
     */
    getDrilldownView: PropTypes.func,

    /**
     * Determines the end date from date prop in the agenda view
     * date prop + length (in number of days) = end date
     */
    length: PropTypes.number,

    /**
     * Show truncated events in an overlay when you click the "+_x_ more" link.
     */
    popup: PropTypes.bool,

    /**
     * Allows mouse selection of ranges of dates/times.
     *
     * The 'ignoreEvents' option prevents selection code from running when a
     * drag begins over an event. Useful when you want custom event click or drag
     * logic
     */
    selectable: PropTypes.oneOf([true, false, 'ignoreEvents']),

    /**
     * Determines the selectable time increments in week and day views, in minutes.
     */
    step: PropTypes.number,

    /**
     * The number of slots per "section" in the time grid views. Adjust with `step`
     * to change the default of 1 hour long groups, with 30 minute slots.
     */
    timeslots: PropTypes.number,

    /**
     * Support to show multi-day events with specific start and end times in the
     * main time grid (rather than in the all day header).
     *
     * **Note: This may cause calendars with several events to look very busy in
     * the week and day views.**
     */
    showMultiDayTimes: PropTypes.bool,

    /**
     * Constrains the minimum _time_ of the Day and Week views.
     */
    min: PropTypes.instanceOf(Date),

    /**
     * Constrains the maximum _time_ of the Day and Week views.
     */
    max: PropTypes.instanceOf(Date),

    /**
     * Determines how far down the scroll pane is initially scrolled down.
     */
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
      <div
        className={clsx(className, 'rbc-calendar')}
        style={style}
      >
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

  /**
   *
   * @param date
   * @param viewComponent
   * @param {'month'|'week'|'work_week'|'day'|'agenda'} [view] - optional
   * parameter. It appears when range change on view changing. It could be handy
   * when you need to have both: range and view type at once, i.e. for manage rbc
   * state via url
   */
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
      action === navigate.TODAY
        ? (today || new Date())
      : action === navigate.DATE
      ? newDate
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

  handleSelectEvent = (...args) => {
    this.props.onSelectEvent?.(...args)
  }

  handleDoubleClickEvent = (...args) => {
    this.props.onDoubleClickEvent?.(...args)
  }

  handleKeyPressEvent = (...args) => {
    this.props.onKeyPressEvent?.(args)
  }

  handleSelectSlot = (slotInfo) => {
    this.props.onSelectSlot?.(slotInfo)
  }

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
