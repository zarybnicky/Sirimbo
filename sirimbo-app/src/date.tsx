import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { format } from 'date-fns';

interface DateRangeProps {
    noYear?: string;
    from: string;
    to: string;
}
export function DateRange({ noYear, from, to }: DateRangeProps) {
    const f = noYear !== undefined ? 'd. M.' : 'd. M. y';
    return <span>{
        (to && from != to)
            ? format(new Date(from), f) + ' - ' + format(new Date(to), f)
            : format(new Date(from), f)
    }</span>;
}
class DateRangeElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<DateRange
            noYear={this.getAttribute('noYear') || undefined}
            from={this.getAttribute('from') || ''}
            to={this.getAttribute('to') || ''}
        />, this);
    }
}
customElements.define('date-range', DateRangeElement);

interface DateProps {
    noYear?: string;
    timestamp?: string;
    date: string;
}
export function DateEl({ noYear, timestamp, date }: DateProps) {
    const f = timestamp !== undefined ? 'd. M. y H:mm' :
        noYear !== undefined ? 'd. M.' : 'd. M. y';
    return <span>{format(new Date(date), f)}</span>;
}
class DateElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<DateEl
            noYear={this.getAttribute('noYear') || undefined}
            timestamp={this.getAttribute('timestamp') || undefined}
            date={this.getAttribute('date') || ''}
        />, this);
    }
}
customElements.define('date', DateElement);
