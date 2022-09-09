import * as React from 'react';
import * as ReactDOM from 'react-dom';
import format from 'date-fns/format';

export function formatDateRange(from: Date, to: Date, noYear?: string) {
  const f = noYear !== undefined ? 'd. M.' : 'd. M. y';
  return (to && from != to)
    ? format(from, f) + ' - ' + format(to, f)
    : format(from, f);
}

interface DateProps {
  noYear?: string;
  timestamp?: string;
  date: Date;
}

interface DateRangeProps {
  noYear?: string;
  from: Date;
  to: Date;
}

export function DateRange({ noYear, from, to }: DateRangeProps) {
  return <span>{formatDateRange(from, to, noYear)}</span>;
}

export function DateEl({ noYear, timestamp, date }: DateProps) {
  const f = timestamp !== undefined ? 'd. M. y H:mm' :
    noYear !== undefined ? 'd. M.' : 'd. M. y';
  return <span>{format(date, f)}</span>;
}

export class DateRangeElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(<DateRange
      noYear={this.getAttribute('noYear') || undefined}
      from={new Date(this.getAttribute('from') || '')}
      to={new Date(this.getAttribute('to') || '')}
    />, this);
  }
}

export class DateElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(<DateEl
      noYear={this.getAttribute('noYear') || undefined}
      timestamp={this.getAttribute('timestamp') || undefined}
      date={new Date(this.getAttribute('date') || '')}
    />, this);
  }
}
