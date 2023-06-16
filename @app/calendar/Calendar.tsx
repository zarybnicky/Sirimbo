import React from 'react'
import clsx from 'clsx'
import { Event, View, Navigate, DragAction, DragDirection, EventInteractionArgs, Resource } from './types'
import { DnDContext, DnDContextType, DnDState } from './DnDContext';
import Month from './views/Month';
import Day from './views/Day';
import Week from './views/Week';
import WorkWeek from './views/WorkWeek';
import Agenda from './views/Agenda';
import { NavigationContext } from 'NavigationContext';

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
  onEventDrop?: (args: EventInteractionArgs) => void;
  onEventResize?: (args: EventInteractionArgs) => void;
  onDropFromOutside?: DnDContextType['draggable']['onDropFromOutside'];
  dragFromOutsideItem?: () => keyof Event | ((event: Event) => Date);
  min?: Date;
  max?: Date;
  resources?: Resource[];
  defaultDate?: Date;
}

export const Calendar = ({
  defaultDate = new Date(),
  onEventDrop,
  onEventResize,
  onDropFromOutside,
  dragFromOutsideItem,
  ...props
}: CalendarProps) => {
  const [view, setView] = React.useState(View.DAY)
  const [date, setDate] = React.useState(defaultDate);
  const [state, setState] = React.useState<DnDState>({ interacting: false });

  const draggableState = React.useMemo<DnDContextType>(() => ({
    draggable: {
      onStart() {
        if (!state.interacting) {
          setState(x => ({ ...x, interacting: true }))
        }
      },
      onEnd(interactionInfo) {
        if (!state.action || !state.event) return
        setState({ action: null, event: null, interacting: false, direction: null })
        if (!interactionInfo) return
        if (state.action === 'move') onEventDrop?.({ ...interactionInfo, event: state.event })
        if (state.action === 'resize') onEventResize?.({ ...interactionInfo, event: state.event })
      },
      onBeginAction(event: Event, action: DragAction, direction: DragDirection|undefined) {
        setState(x => ({ ...x, event, action, direction }))
      },
      onDropFromOutside,
      dragFromOutsideItem,
      dragAndDropAction: state,
    },
  }), [state, date, view]);

  const ViewComponent = VIEWS[view];

  const navigationContext: NavigationContext = {
    onNavigate(action: Navigate, newDate?: Date) {
      newDate = newDate || date || new Date()
      setDate(action === Navigate.TODAY ? new Date() : action === Navigate.DATE ? newDate : ViewComponent.navigate(newDate, action));
    },
    onDrillDown(date, view) {
      setView(view);
      setDate(date);
    },
  };

  return (
    <DnDContext.Provider value={draggableState}>
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
