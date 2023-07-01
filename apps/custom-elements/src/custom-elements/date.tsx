import * as React from 'react';
import * as ReactDOM from 'react-dom';
import format from 'date-fns/format';

export function formatDateRange(from: string, to: string, noYear?: string) {
    const f = noYear !== undefined ? 'd. M.' : 'd. M. y';
    return (to && from != to)
        ? format(new Date(from), f) + ' - ' + format(new Date(to), f)
        : format(new Date(from), f);
}

interface DateProps {
    noYear?: string;
    timestamp?: string;
    date: string;
}

interface DateRangeProps {
    noYear?: string;
    from: string;
    to: string;
}

export function DateRange({ noYear, from, to }: DateRangeProps) {
    return <span>{formatDateRange(from, to, noYear)}</span>;
}

export function DateEl({ noYear, timestamp, date }: DateProps) {
    const f = timestamp !== undefined ? 'd. M. y H:mm' :
        noYear !== undefined ? 'd. M.' : 'd. M. y';
    return <span>{format(new Date(date), f)}</span>;
}

export class DateRangeElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<DateRange
            noYear={this.getAttribute('noYear') || undefined}
            from={this.getAttribute('from') || ''}
            to={this.getAttribute('to') || ''}
        />, this);
    }
}
