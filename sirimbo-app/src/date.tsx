import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { format } from 'date-fns';

interface DateRangeProps {
    year?: string;
    from: string;
    to: string;
}

export function DateRange({ year, from, to }: DateRangeProps) {
    const f = year !== undefined ? 'd.M.y' : 'd.M.';
    return <span>{
        (to && from != to)
            ? format(new Date(from), f) + ' - ' + format(new Date(to), f)
            : format(new Date(from), f)
    }</span>;
}

class DateRangeElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<DateRange
            year={this.getAttribute('year') || undefined}
            from={this.getAttribute('from') || ''}
            to={this.getAttribute('to') || ''}
        />, this);
    }
}
customElements.define('date-range', DateRangeElement);
