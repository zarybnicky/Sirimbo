import React from 'react';
import type { ViewProps } from '@/calendar/types';
import Month from '@/calendar/views/Month';
import { add, endOf, startOf } from 'date-arithmetic';
import { format, range } from '@/calendar/localizer';
import TimeGrid from '@/calendar/TimeGrid';
import { fullDateFormatter } from '@/ui/format';
import Agenda from '@/calendar/views/Agenda';

export type CalendarView = {
  component: React.ComponentType<ViewProps>;
  range: (d: Date) => Date[];
  nav: (d: Date, dir: -1 | 1) => Date;
  label: (d: Date) => string;
  supportsGrouping: boolean;
};

export const CalendarViews = {
  month: {
    component: Month,
    range: (d: Date) => {
      const first = startOf(startOf(d, 'month'), 'week', 1);
      const last = endOf(endOf(d, 'month'), 'week', 1);
      return range(first, last, 'day');
    },
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'month'),
    label: (d: Date) => format(d, 'MMMM yyyy'),
    supportsGrouping: false,
  },
  week: {
    component: TimeGrid,
    range: (d: Date) => range(startOf(d, 'week', 1), endOf(d, 'week', 1)),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'week'),
    label: (d: Date) => {
      const s = startOf(d, 'week', 1);
      const e = endOf(d, 'week', 1);
      return fullDateFormatter.formatRange(s, e).replace(' – ', ' – ');
    },
    supportsGrouping: true,
  },
  work_week: {
    component: TimeGrid,
    range: (d: Date) =>
      range(startOf(d, 'week', 1), endOf(d, 'week', 1)).filter(
        (x) => ![0, 6].includes(x.getDay()),
      ),
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'week'),
    label: (d: Date) => {
      const s = startOf(d, 'week', 1);
      const e = endOf(d, 'week', 1);
      return fullDateFormatter.formatRange(s, e).replace(' – ', ' – ');
    },
    supportsGrouping: true,
  },
  day: {
    component: TimeGrid,
    range: (d: Date) => [startOf(d, 'day')],
    nav: (d: Date, dir: -1 | 1) => add(d, dir, 'day'),
    label: (d: Date) => format(d, 'cccc dd. MM. yyyy'),
    supportsGrouping: true,
  },
  agenda: {
    component: Agenda,
    range: (d: Date) => range(d, add(d, 6, 'day'), 'day'),
    nav: (d: Date, dir: -1 | 1) => add(d, 7 * dir, 'day'),
    label: (d: Date) =>
      fullDateFormatter.formatRange(d, add(d, 6, 'day')).replace(' – ', ' – '),
    supportsGrouping: false,
  },
} as const satisfies Record<string, CalendarView>;

export type CalendarViewKey = keyof typeof CalendarViews;
