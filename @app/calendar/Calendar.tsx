import React from 'react'
import clsx from 'clsx'
import { Event, View, Navigate, DragAction, DragDirection, Resource } from './types'
import { DnDContext, DnDContextType, DnDState } from './DnDContext';
import Month from './views/Month';
import Day from './views/Day';
import Week from './views/Week';
import WorkWeek from './views/WorkWeek';
import Agenda from './views/Agenda';
import { NavigationContext } from 'NavigationContext';
import { endOf, startOf } from 'date-arithmetic';

const VIEWS = {
  [View.MONTH]: Month,
  [View.WEEK]: Week,
  [View.WORK_WEEK]: WorkWeek,
  [View.DAY]: Day,
  [View.AGENDA]: Agenda,
};

export interface CalendarProps {
  events?: Event[];
  backgroundEvents?: Event[];
  onDropFromOutside?: DnDContextType['draggable']['onDropFromOutside'];
  dragFromOutsideItem?: () => keyof Event | ((event: Event) => Date);
  min?: Date;
  max?: Date;
  resources?: Resource[];
  defaultDate?: Date;
}

export const Calendar = ({
  defaultDate = new Date(),
  ...props
}: CalendarProps) => {
  const [view, setView] = React.useState(View.DAY)
  const [date, setDate] = React.useState(defaultDate);
  const draggableState = React.useRef<DnDState>({ interacting: false });

  const draggableContext = React.useMemo<DnDContextType>(() => ({
    draggable: {
      onStart() {
        draggableState.current = { ...draggableState.current, interacting: true };
      },
      onEnd(interactionInfo) {
        const { event, action } = draggableState.current;
        draggableState.current = { action: null, event: null, interacting: false, direction: null };
        if (!action || !event || !interactionInfo) return
        if (action === 'move') {
          // TODO: onDrop
        }
        if (action === 'resize') {
          // TODO: onResize
        }
      },
      onBeginAction(event: Event, action: DragAction, direction: DragDirection|undefined) {
        draggableState.current = { action, event, interacting: true, direction };
      },
      onDropFromOutside({ start, end, allDay, resourceId }) {
        // TODO: onDrop
      },
      dragFromOutsideItem() {
        // TODO: dragFromOutside
        return undefined
      },
      dragAndDropAction: draggableState,
    },
  }), []);

  const ViewComponent = VIEWS[view];

  const navigationContext: NavigationContext = React.useMemo<NavigationContext>(() => ({
    timeslots: 4,
    step: 15,
    min: startOf(new Date(), 'day'),
    max: endOf(new Date(), 'day'),
    focusedTime: new Date(1972, 0, 1, 16, 0, 0),
    onNavigate(action: Navigate, newDate?: Date) {
      setDate((oldDate) => {
        if (action === Navigate.TODAY) {
          return new Date()
        }
        if (action === Navigate.DATE) {
          return newDate || oldDate || new Date();
        }
        return ViewComponent.navigate(newDate || oldDate || new Date(), action);
      })
    },
    onDrillDown(date, view) {
      setView(view);
      setDate(date);
    },
  }), [view]);

  return (
    <DnDContext.Provider value={draggableContext}>
      <NavigationContext.Provider value={navigationContext}>
      <div className={clsx('rbc-calendar rbc-addons-dnd', !!state.interacting && 'rbc-addons-dnd-is-dragging')}>
        <div className="rbc-toolbar">
          <span className="rbc-btn-group">
            <button type="button" onClick={() => navigationContext.onNavigate(Navigate.TODAY)}>Dnes</button>
            <button type="button" onClick={() => navigationContext.onNavigate(Navigate.PREVIOUS)}>Zpět</button>
            <button type="button" onClick={() => navigationContext.onNavigate(Navigate.NEXT)}>Dále</button>
          </span>

          <span className="rbc-toolbar-label">
            {ViewComponent.title(date)}
          </span>

          <span className="rbc-btn-group">
            {Object.values(View).map((name) => (
              <button type="button" key={name} className={clsx({ 'rbc-active': view === name })} onClick={setView.bind(null, name)}>
                {VIEWS[name].name}
              </button>
            ))}
          </span>
        </div>

          <ViewComponent
            date={date}
            events={props.events || []}
            backgroundEvents={props.backgroundEvents || []}
            resources={props.resources || []}
          />
        </div>
      </NavigationContext.Provider>
    </DnDContext.Provider>
  )
}
