// Type definitions for react-big-calendar 1.6
// Project: https://github.com/jquense/react-big-calendar
// Definitions by: Piotr Witek <https://github.com/piotrwitek>
//                 Austin Turner <https://github.com/paustint>
//                 Krzysztof Bezrąk <https://github.com/pikpok>
//                 Sebastian Silbermann <https://github.com/eps1lon>
//                 Paul Potsides <https://github.com/strongpauly>
//                 janb87 <https://github.com/janb87>
//                 Panagiotis Rikarnto Siavelis <https://github.com/siavelis>
//                 Lucas Silva Souza <https://github.com/lksilva>
//                 Siarhey Belofost <https://github.com/SergeyBelofost>
//                 Mark Nelissen <https://github.com/marknelissen>
//                 Paito Anderson <https://github.com/PaitoAnderson>
//                 Jan Michalak <https://github.com/michalak111>
//                 Tom Price <https://github.com/tomtom5152>
//                 Daniele Carrucciu <https://github.com/catruzz>
//                 Chris Frewin <https://github.com/princefishthrower>
//                 decimoseptimo <https://github.com/decimoseptimo>
// Definitions: https://github.com/DefinitelyTyped/DefinitelyTyped
// TypeScript Version: 2.8
import * as React from 'react';

type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>;

export type stringOrDate = string | Date; // this isn't documented in the official repo, a thorough review is needed as to where stringOrDate or Date applies

export type ViewKey = 'MONTH' | 'WEEK' | 'WORK_WEEK' | 'DAY' | 'AGENDA';
export type View = 'month' | 'week' | 'work_week' | 'day' | 'agenda';
export type ViewProps<TEvent extends object = Event, TResource extends object = object> = Omit<
    CalendarProps<TEvent, TResource>,
    'className' | 'style' | 'view'
> & {
    date: stringOrDate; // date has always a value, in contrast to optional date in CalendarProps

    // props assigned from Calendar instance, see there if you want to improve the type defs:
    getDrilldownView: any; // = this.getDrilldownView
    onNavigate: any; // = this.handleNavigate
    onDrillDown: any; // = this.handleDrillDown
    onSelectEvent: any; // = this.handleSelectEvent
    onDoubleClickEvent: any; // = this.handleDoubleClickEvent
    onSelectSlot: any; // = this.handleSelectSlot
};
export type ViewsProps =
    | View[]
    | {
          work_week?: boolean | (React.ComponentType<any> & ViewStatic) | undefined;
          day?: boolean | (React.ComponentType<any> & ViewStatic) | undefined;
          agenda?: boolean | (React.ComponentType<any> & ViewStatic) | undefined;
          month?: boolean | (React.ComponentType<any> & ViewStatic) | undefined;
          week?: boolean | (React.ComponentType<any> & ViewStatic) | undefined;
      };
export type DayLayoutFunction<TEvent extends object = Event> = (_: {
    events: TEvent[];
    minimumStartDifference: number;
    slotMetrics: any;
}) => Array<{ event: TEvent; style: React.CSSProperties }>;
export type DayLayoutAlgorithm = 'overlap' | 'no-overlap';
export type NavigateAction = 'PREV' | 'NEXT' | 'TODAY' | 'DATE';
export interface Event {
    allDay?: boolean | undefined;
    title?: React.ReactNode | undefined;
    start?: Date | undefined;
    end?: Date | undefined;
    resource?: any;
}
export interface DateRange {
    start: Date;
    end: Date;
}

export interface SlotInfo {
    start: Date;
    end: Date;
    slots: Date[];
    action: 'select' | 'click' | 'doubleClick';
    /** For "TimeGrid" views */
    resourceId?: number | string | undefined;
    /** For "select" action */
    bounds?:
        | {
              x: number;
              y: number;
              top: number;
              bottom: number;
              left: number;
              right: number;
          }
        | undefined;
    /** For "click" or "doubleClick" actions */
    box?:
        | {
              x: number;
              y: number;
              clientX: number;
              clientY: number;
          }
        | undefined;
}

export type FormatInput = number | string | Date;

export interface CalendarProps<TEvent extends object = Event, TResource extends object = object> {
    children?: React.ReactNode;
    ref?: React.LegacyRef<Calendar<TEvent, TResource>> | undefined;

    date?: stringOrDate | undefined;
    view?: View | undefined;
    events?: TEvent[] | undefined;
    backgroundEvents?: TEvent[] | undefined;
    handleDragStart?: ((event: TEvent) => void) | undefined;
    onNavigate?: ((newDate: Date, view: View, action: NavigateAction) => void) | undefined;
    onView?: ((view: View) => void) | undefined;
    onDrillDown?: ((date: Date, view: View) => void) | undefined;
    onSelectSlot?: ((slotInfo: SlotInfo) => void) | undefined;
    onDoubleClickEvent?: ((event: TEvent, e: React.SyntheticEvent<HTMLElement>) => void) | undefined;
    onSelectEvent?: ((event: TEvent, e: React.SyntheticEvent<HTMLElement>) => void) | undefined;
    onKeyPressEvent?: ((event: TEvent, e: React.SyntheticEvent<HTMLElement>) => void) | undefined;
    onSelecting?: (range: { start: Date; end: Date }) => boolean | undefined;
    onRangeChange?: (range: Date[] | { start: Date; end: Date }, view?: View) => void | undefined;
    selected?: any;
    views?: ViewsProps | undefined;
    doShowMoreDrillDown?: boolean | undefined;
    drilldownView?: View | null | undefined;
    getDrilldownView?:
        | ((targetDate: Date, currentViewName: View) => void)
        | null
        | undefined;
    length?: number | undefined;
    popup?: boolean | undefined;
    selectable?: boolean | 'ignoreEvents' | undefined;
    step?: number | undefined;
    timeslots?: number | undefined;
    showMultiDayTimes?: boolean | undefined;
    min?: Date | undefined;
    max?: Date | undefined;
    scrollToTime?: Date | undefined;
    dayLayoutAlgorithm?: DayLayoutAlgorithm | DayLayoutFunction<TEvent> | undefined;
    resources?: TResource[] | undefined;
    defaultView?: View | undefined;
    defaultDate?: stringOrDate | undefined;
    className?: string | undefined;
    style?: React.CSSProperties | undefined;
    onShowMore?: ((events: TEvent[], date: Date) => void) | undefined;
}

export interface TitleOptions {
    [propName: string]: any;
}

export interface ViewStatic {
    navigate(date: Date, action: NavigateAction, props: any): Date;
    title(date: Date, options: TitleOptions): string;
}

export interface MoveOptions {
    action: NavigateAction;
    date: Date;
    today: Date;
}

export class Calendar<TEvent extends object = Event, TResource extends object = object> extends React.Component<
    CalendarProps<TEvent, TResource>
> {}

export const Navigate: {
    PREVIOUS: 'PREV';
    NEXT: 'NEXT';
    TODAY: 'TODAY';
    DATE: 'DATE';
};
export const Views: {
    MONTH: 'month';
    WEEK: 'week';
    WORK_WEEK: 'work_week';
    DAY: 'day';
    AGENDA: 'agenda';
};
export function move(View: ViewStatic | ViewKey, options: MoveOptions): Date;

export interface TimeGridProps<TEvent extends object = Event, TResource extends object = object> {
    eventOffset: number;
    events?: TEvent[] | undefined;
    backgroundEvents?: TEvent[] | undefined;
    resources?: TResource[] | undefined;
    step?: number | undefined;
    timeslots?: number | undefined;
    range?: any[] | undefined;
    min?: Date | undefined;
    max?: Date | undefined;
    scrollToTime?: Date | undefined;
    showMultiDayTimes?: boolean | undefined;
    width?: number | undefined;
    selected?: object | undefined;
    selectable?: boolean | 'ignoreEvents' | undefined;
    onNavigate?: ((action: NavigateAction) => void) | undefined;
    onSelectSlot?: ((slotInfo: SlotInfo) => void) | undefined;
    onSelectEnd?: ((...args: any[]) => any) | undefined;
    onSelectStart?: ((...args: any[]) => any) | undefined;
    onSelectEvent?: ((event: TEvent, e: React.SyntheticEvent<HTMLElement>) => void) | undefined;
    onDoubleClickEvent?: ((event: TEvent, e: React.SyntheticEvent<HTMLElement>) => void) | undefined;
    onKeyPressEvent?: ((...args: any[]) => any) | undefined;
    onDrillDown?: ((date: Date, view: View) => void) | undefined;
    getDrilldownView?:
        | ((targetDate: Date, currentViewName: View) => void)
        | null
        | undefined;
    dayLayoutAlgorithm?: any;
}

export class TimeGrid<TEvent extends object = Event, TResource extends object = object> extends React.Component<
    TimeGridProps<TEvent, TResource>
> {}

export interface WorkWeekProps {
    date: Date;
}

export class WorkWeek extends Week {}

export interface WeekProps {
    date: Date;
}

export class Week extends React.Component<WeekProps> {
    static range: (date: Date) => Date[];
    static navigate: (date: Date, action: NavigateAction) => Date;
    static title: (date: Date) => string;
}

export interface DayProps {
    date: Date;
}
export class Day extends React.Component<DayProps> {}

// Turn off automatic exports
export {};
