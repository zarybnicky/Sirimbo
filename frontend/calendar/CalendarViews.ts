import React from 'react';
import type { DateRange, ViewProps } from '@/calendar/types';
import Month from '@/calendar/Month';
import { add, endOf, startOf } from 'date-arithmetic';
import { format } from '@/calendar/localizer';
import TimeGrid from '@/calendar/TimeGrid';
import { fullDateFormatter } from '@/ui/format';
import Agenda from '@/calendar/Agenda';

export type CalendarView = {
  component: React.ComponentType<ViewProps>;
  range: (d: Date) => DateRange;
  nav: (d: Date, dir: -1 | 1) => Date;
  label: (range: DateRange) => string;
  supportsGrouping: boolean;
};

export const CalendarViews = {
  month: {
    component: Month,
    range: (d: Date) => ({
      since: startOf(startOf(d, 'month'), 'week', 1),
      until: endOf(endOf(d, 'month'), 'week', 1),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'month'),
    label: ({ since }) => format(since, 'MMMM yyyy'),
    supportsGrouping: false,
  },
  week: {
    component: TimeGrid,
    range: (d: Date) => ({
      since: startOf(d, 'week', 1),
      until: endOf(d, 'week', 1),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'week'),
    label: ({ since, until }) =>
      fullDateFormatter.formatRange(since, until).replace(' – ', ' – '),
    supportsGrouping: true,
  },
  work_week: {
    component: TimeGrid,
    range: (d: Date) => ({
      since: startOf(d, 'week', 1),
      until: add(endOf(d, 'week', 1), -2, 'day'),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'week'),
    label: ({ since, until }) =>
      fullDateFormatter.formatRange(since, until).replace(' – ', ' – '),
    supportsGrouping: true,
  },
  day: {
    component: TimeGrid,
    range: (d: Date) => ({
      since: startOf(d, 'day'),
      until: startOf(d, 'day'),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'day'),
    label: ({ since }) => format(since, 'cccc dd. MM. yyyy'),
    supportsGrouping: true,
  },
  agenda: {
    component: Agenda,
    range: (d: Date) => ({
      since: d,
      until: add(d, 6, 'day'),
    }),
    nav: (d: Date, dir: -1 | 1) => add(d, 7 * dir, 'day'),
    label: ({ since, until }) =>
      fullDateFormatter.formatRange(since, until).replace(' – ', ' – '),
    supportsGrouping: false,
  },
} as const satisfies Record<string, CalendarView>;

export type CalendarViewKey = keyof typeof CalendarViews;
