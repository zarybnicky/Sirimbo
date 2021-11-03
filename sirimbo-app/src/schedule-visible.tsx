import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';

import Form from 'react-bootstrap/Form';
import * as queries from './queries';
import { createClient } from './client';

interface Props {
    id: number;
}

const ScheduleVisible = ({ id }: Props) => {
    const { loading, error, data, refetch } = useQuery(queries.RozpisDocument, {
        variables: { id },
    });
    const [toggleVisible] = useMutation(queries.SetRozpisVisibleDocument, {
        onCompleted: () => refetch(),
    });
    const visible = data?.rozpis_by_pk?.r_visible || false;
    const toggle = () => toggleVisible({ variables: { id, visible: !visible } });
    if (error) {
        console.error(error);
    }
    return loading ? null : <Form.Check name={id.toString()} checked={visible} onChange={toggle} />
};

const ReservationVisible = ({ id }: Props) => {
    const { loading, error, data, refetch } = useQuery(queries.NabidkaDocument, {
        variables: { id },
    });
    const [toggleVisible] = useMutation(queries.SetNabidkaVisibleDocument, {
        onCompleted: () => refetch(),
    });
    const visible = data?.nabidka_by_pk?.n_visible || false;
    const toggle = () => toggleVisible({ variables: { id, visible: !visible } });
    if (error) {
        console.error(error);
    }
    return loading ? null : <Form.Check name={id.toString()} checked={visible} onChange={toggle} />
};

class ScheduleVisibleElement extends HTMLElement {
    connectedCallback() {
        const id = parseInt(this.getAttribute('name')!!, 10);
        const type = this.getAttribute('type');
        const component =
            type === 'schedule' ? <ScheduleVisible id={id}></ScheduleVisible> :
                type === 'reservation' ? <ReservationVisible id={id}></ReservationVisible>
                    : null;
        ReactDOM.render(
            <ApolloProvider client={createClient()}>{component}</ApolloProvider>,
            this
        );
    }
}
customElements.define('visibility-checkbox', ScheduleVisibleElement);
