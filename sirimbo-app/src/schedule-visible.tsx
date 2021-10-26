import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';

import Form from 'react-bootstrap/Form';
import { RozpisDocument, SetRozpisVisibleDocument } from './queries';
import { client } from './client';

const ScheduleVisible = ({ id }: { id: number; }) => {
    const { error, data, refetch } = useQuery(RozpisDocument, { variables: { id } });
    const [toggleVisible] = useMutation(SetRozpisVisibleDocument, { onCompleted: () => refetch() });
    const visible = data?.rozpis_by_pk?.r_visible || false;
    const toggle = () => toggleVisible({ variables: { id, visible: !visible } });
    if (error) {
        console.error(error);
    }
    return <Form.Check name={id} checked={visible} onChange={toggle} />
};

class ScheduleVisibleElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={client}>
                <ScheduleVisible id={parseInt(this.getAttribute('name'), 10)}></ScheduleVisible>
            </ApolloProvider>,
            this
        );
    }
}
customElements.define('schedule-visible', ScheduleVisibleElement);
