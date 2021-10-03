import * as React from 'react';
import { useState } from 'react';
import * as ReactDOM from 'react-dom';
import Form from 'react-bootstrap/Form';

const text2bool = (x: string) => x === '0' ? false : !!x;

interface CheckboxProps {
    type: 'event-checkbox' | 'schedule-checkbox' | '';
    name: string;
    initialValue: string;
}

export function VisibilityCheckbox({ type, name, initialValue }: CheckboxProps) {
    const [checked, setChecked] = useState(text2bool(initialValue));
    const toggle = async () => {
        const res = await fetch(`/api/${type.replace('-checkbox', '')}/${name}/toggle-visible`);
        setChecked(await res.json() as boolean);
    };
    return <Form.Check name={name} checked={checked} onChange={toggle} />
}
class VisibilityCheckboxElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<VisibilityCheckbox
            type={this.tagName as any}
            name={this.getAttribute('name') || ''}
            initialValue={this.getAttribute('initialValue') || '0'}
        />, this);
    }
}
customElements.define('event-checkbox', VisibilityCheckboxElement);
customElements.define('schedule-checkbox', VisibilityCheckboxElement);
customElements.define('reservation-checkbox-checkbox', VisibilityCheckboxElement);
