import React from 'react';
import type { DateRange, ViewProps } from '@/calendar/types';
import Month from '@/calendar/Month';
import {
  addDays,
  addMonths,
  addWeeks,
  endOfMonth,
  endOfWeek,
  startOfDay,
  startOfMonth,
  startOfWeek,
} from 'date-fns';
import TimeGrid from '@/calendar/TimeGrid';
import { fullDateFormatter, monthFormatter } from '@/ui/format';
import Agenda from '@/calendar/Agenda';

export type CalendarView = {
  component: React.ComponentType<ViewProps>;
  range: (d: Date) => DateRange;
  nav?: (d: Date, dir: -1 | 1) => Date;
  label: (range: DateRange) => string;
  supportsGrouping: boolean;
};

export const CalendarViews = {
  month: {
    component: Month,
    range: (d: Date) => ({
      since: startOfWeek(startOfMonth(d), { weekStartsOn: 1 }),
      until: endOfWeek(endOfMonth(d), { weekStartsOn: 1 }),
    }),
    nav: (d: Date, dir: -1 | 1) => addMonths(d, dir),
    label: ({ since }) => monthFormatter.format(addWeeks(since, 1)),
    supportsGrouping: false,
  },
  week: {
    component: TimeGrid,
    range: (d: Date) => ({
      since: startOfWeek(d, { weekStartsOn: 1 }),
      until: endOfWeek(d, { weekStartsOn: 1 }),
    }),
    nav: (d: Date, dir: -1 | 1) => addWeeks(d, dir),
    label: ({ since, until }) =>
      fullDateFormatter.formatRange(since, until).replace(' – ', ' – '),
    supportsGrouping: true,
  },
  work_week: {
    component: TimeGrid,
    range: (d: Date) => ({
      since: startOfWeek(d, { weekStartsOn: 1 }),
      until: addDays(endOfWeek(d, { weekStartsOn: 1 }), -2),
    }),
    nav: (d: Date, dir: -1 | 1) => addWeeks(d, dir),
    label: ({ since, until }) =>
      fullDateFormatter.formatRange(since, until).replace(' – ', ' – '),
    supportsGrouping: true,
  },
  day: {
    component: TimeGrid,
    range: (d: Date) => ({
      since: startOfDay(d),
      until: startOfDay(d),
    }),
    nav: (d: Date, dir: -1 | 1) => addDays(d, dir),
    label: ({ since }) => fullDateFormatter.format(since),
    supportsGrouping: true,
  },
  agenda: {
    component: Agenda,
    range: (d: Date) => ({
      since: d,
      until: addDays(d, 6),
    }),
    nav: (d: Date, dir: -1 | 1) => addWeeks(d, dir),
    label: ({ since, until }) =>
      fullDateFormatter.formatRange(since, until).replace(' – ', ' – '),
    supportsGrouping: false,
  },
} as const satisfies Record<string, CalendarView>;

export type CalendarViewKey = keyof typeof CalendarViews | 'range';
