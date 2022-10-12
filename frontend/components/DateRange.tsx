import * as React from 'react';
import format from 'date-fns/format';

interface DateRangeProps {
  noYear?: boolean;
  from: string;
  to?: string;
}

export function DateRange({ noYear, from, to }: DateRangeProps) {
  const f = noYear ? 'd. M.' : 'd. M. y';
  if (to && from != to) {
    return <>{format(new Date(from), f) + ' - ' + format(new Date(to), f)}</>;
  }
  return <>{format(new Date(from), f)}</>;
}
