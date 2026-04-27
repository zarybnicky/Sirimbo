import React from 'react';
import type { ViewProps } from '@/calendar/types';
import Month from '@/calendar/Month';
import { add, endOf, startOf } from 'date-arithmetic';
import { format } from '@/calendar/localizer';
import TimeGrid from '@/calendar/TimeGrid';
import { fullDateFormatter } from '@/ui/format';
import Agenda from '@/calendar/Agenda';

export type CalendarView = {
  component: React.ComponentType<ViewProps>;
  range: (d: Date) => { start: Date; end: Date };
  nav: (d: Date, dir: -1 | 1) => Date;
  label: (range: { start: Date; end: Date }) => string;
  supportsGrouping: boolean;
};

export const CalendarViews = {
  month: {
    component: Month,
    range: (d: Date) => ({
      start: startOf(startOf(d, 'month'), 'week', 1),
      end: endOf(endOf(d, 'month'), 'week', 1),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'month'),
    label: ({ start }) => format(start, 'MMMM yyyy'),
    supportsGrouping: false,
  },
  week: {
    component: TimeGrid,
    range: (d: Date) => ({
      start: startOf(d, 'week', 1),
      end: endOf(d, 'week', 1),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'week'),
    label: ({ start, end }) =>
      fullDateFormatter.formatRange(start, end).replace(' – ', ' – '),
    supportsGrouping: true,
  },
  work_week: {
    component: TimeGrid,
    range: (d: Date) => ({
      start: startOf(d, 'week', 1),
      end: add(endOf(d, 'week', 1), -2, 'day'),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'week'),
    label: ({ start, end }) =>
      fullDateFormatter.formatRange(start, end).replace(' – ', ' – '),
    supportsGrouping: true,
  },
  day: {
    component: TimeGrid,
    range: (d: Date) => ({
      start: startOf(d, 'day'),
      end: startOf(d, 'day'),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'day'),
    label: ({ start }) => format(start, 'cccc dd. MM. yyyy'),
    supportsGrouping: true,
  },
  agenda: {
    component: Agenda,
    range: (d: Date) => ({
      start: d,
      end: add(d, 6, 'day'),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, 7 * dir, 'day'),
    label: ({ start, end }) =>
      fullDateFormatter.formatRange(start, end).replace(' – ', ' – '),
    supportsGrouping: false,
  },
} as const satisfies Record<string, CalendarView>;

export type CalendarViewKey = keyof typeof CalendarViews;
