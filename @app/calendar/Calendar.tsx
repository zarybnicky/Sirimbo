import clsx from 'clsx';
import React from 'react';
import { DndProvider } from './DnDContext';
import { add, agendaHeaderFormat, dayRangeHeaderFormat, endOf, format, startOf, startOfWeek } from './localizer';
import { NavigationProvider } from './NavigationContext';
import { Event, Navigate, Resource, View } from './types';
import Agenda from './views/Agenda';
import Day from './views/Day';
import Month from './views/Month';
import Week from './views/Week';
import WorkWeek from './views/WorkWeek';

export interface CalendarProps {
  events?: Event[];
  backgroundEvents?: Event[];
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
  const [isDragging, setIsDragging] = React.useState(false);

  const ViewComponent = {
    [View.MONTH]: Month,
    [View.WEEK]: Week,
    [View.WORK_WEEK]: WorkWeek,
    [View.DAY]: Day,
    [View.AGENDA]: Agenda,
  }[view];

  return (
    <DndProvider setIsDragging={setIsDragging}>
      <NavigationProvider setDate={setDate} setView={setView}>
        <div className={clsx('rbc-calendar rbc-addons-dnd', isDragging && 'rbc-addons-dnd-is-dragging')}>
          <div className="rbc-toolbar">
            <span className="rbc-btn-group">
              <button type="button" onClick={() => setDate(new Date())}>Dnes</button>
              <button type="button" onClick={() => setDate(ViewComponent.navigate(date, Navigate.PREVIOUS))}>Zpět</button>
              <button type="button" onClick={() => setDate(ViewComponent.navigate(date, Navigate.NEXT))}>Dále</button>
            </span>

            <span className="rbc-toolbar-label">
              {view === View.MONTH ? format(date, 'MMMM yyyy') :
               view === View.DAY ? format(date, 'cccc MMM dd') :
               [View.WORK_WEEK, View.WEEK].includes(view) ? dayRangeHeaderFormat({ start: startOf(date, 'week', startOfWeek), end: endOf(date, 'week', startOfWeek) }) :
               view === View.AGENDA ? agendaHeaderFormat({ start: date, end: add(date, 7, 'day') }) :
               ''}
            </span>

            <span className="rbc-btn-group">
              {Object.values(View).map((name) => (
                <button type="button" key={name} className={clsx({ 'rbc-active': view === name })} onClick={setView.bind(null, name)}>
                  {view === View.MONTH ? 'Měsíc' :
                   view === View.DAY ? 'Den' :
                   view === View.WEEK ? 'Týden' :
                   view === View.WORK_WEEK ? 'Pracovní dny' :
                   view === View.AGENDA ? 'Agenda' :
                   ''}
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
      </NavigationProvider>
    </DndProvider>
  )
}
