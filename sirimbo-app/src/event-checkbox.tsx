import * as React from 'react';
import * as ReactDOM from 'react-dom';

export function EventCheckbox() {
    return <div className="user-card">
        <h1>Hello</h1>
    </div>
}

class EventCheckboxElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<EventCheckbox />, this);
    }
}
customElements.define('event-checkbox', EventCheckboxElement);
