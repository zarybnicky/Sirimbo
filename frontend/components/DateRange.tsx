import * as React from 'react';
import { formatDateRange } from 'lib/format-date-range';

interface DateRangeProps {
  noYear?: string;
  from: Date;
  to?: Date;
}

export function DateRange({ noYear, from, to }: DateRangeProps) {
  return <span>{formatDateRange(from, to, noYear)}</span>;
}
