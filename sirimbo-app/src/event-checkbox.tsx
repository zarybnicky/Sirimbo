import * as React from 'react';
import { useState } from 'react';
import * as ReactDOM from 'react-dom';
import Form from 'react-bootstrap/Form';


const text2bool = (x: string) => x === '0' ? false : !!x;
interface EventCheckboxProps {
    name: string;
    initialValue: string;
}
export function EventCheckbox({ name, initialValue }: EventCheckboxProps) {
    const [checked, setChecked] = useState(text2bool(initialValue));
    console.log(checked, initialValue);
    const toggle = async () => {
        const res = await fetch(`/api/event/${name}/toggle-visible`);
        setChecked(await res.json() as boolean);
    };
    return <Form.Check name={name} checked={checked} onChange={toggle} />
}

class EventCheckboxElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<EventCheckbox
            name={this.getAttribute('name') || ''}
            initialValue={this.getAttribute('initialValue') || '0'}
        />, this);
    }
}
customElements.define('event-checkbox', EventCheckboxElement);
