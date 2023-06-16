import React from 'react'
import clsx from 'clsx'
import { Event, View, Navigate, DragAction, DragDirection, EventInteractionArgs, Resource, SlotInfo } from './utils/constants'
import { DnDContext, DnDContextType, DnDState } from './DnDContext';
import Month from './Month';
import Day from './Day';
import Week from './Week';
import WorkWeek from './WorkWeek';
import Agenda from './Agenda';

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
  onSelectEvent?: (event: Event) => void;
  onSelectSlot?: (slotInfo: SlotInfo) => void;
  onEventDrop?: (args: EventInteractionArgs) => void;
  onEventResize?: (args: EventInteractionArgs) => void;
  onDropFromOutside?: DnDContextType['draggable']['onDropFromOutside'];
  dragFromOutsideItem?: () => keyof Event | ((event: Event) => Date);
  length?: number;
  min?: Date;
  max?: Date;
  resources?: Resource[];
  defaultDate?: Date;
}

export const Calendar = ({
  events = [],
  backgroundEvents = [],
  resources = [],
  length,
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

  const ViewComponent = VIEWS[view];

  const handleNavigate = (action: Navigate, newDate?: Date) => {
    newDate = newDate || date || new Date()
    setDate(action === Navigate.TODAY ? new Date() : action === Navigate.DATE ? newDate : ViewComponent.navigate(newDate, action, length));
  }

  return (
    <DnDContext.Provider value={{
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
        onBeginAction(event: Event, action: DragAction, direction: DragDirection) {
          setState(x => ({ ...x, event, action, direction }))
        },
        onDropFromOutside,
        dragFromOutsideItem,
        dragAndDropAction: state,
      },
    } as DnDContextType}>
      <div className={clsx('rbc-calendar rbc-addons-dnd', !!state.interacting && 'rbc-addons-dnd-is-dragging')}>
        <div className="rbc-toolbar">
          <span className="rbc-btn-group">
            <button type="button" onClick={() => handleNavigate(Navigate.TODAY)}>Dnes</button>
            <button type="button" onClick={() => handleNavigate(Navigate.PREVIOUS)}>Zpět</button>
            <button type="button" onClick={() => handleNavigate(Navigate.NEXT)}>Dále</button>
          </span>

          <span className="rbc-toolbar-label">
            {ViewComponent.title(date, length)}
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
          {...props}
          events={events}
          backgroundEvents={backgroundEvents}
          resources={resources}
          date={date}
          length={length}
          onNavigate={handleNavigate}
          onDrillDown={(date: Date, view: View) => {
            setView(view)
            setDate(date)
          }}
        />
      </div>
    </DnDContext.Provider>
  )
}
