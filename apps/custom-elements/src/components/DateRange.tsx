import * as React from 'react';
import format from 'date-fns/format';

interface DateRangeProps {
  noYear?: string;
  from: string;
  to: string;
}

export function formatDateRange(from: string, to: string, noYear?: string) {
  const f = noYear !== undefined ? 'd. M.' : 'd. M. y';
  return (to && from != to)
    ? format(new Date(from), f) + ' - ' + format(new Date(to), f)
    : format(new Date(from), f);
}

export function DateRange({ noYear, from, to }: DateRangeProps) {
  return <span>{formatDateRange(from, to, noYear)}</span>;
}
