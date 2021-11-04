import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { gql } from 'graphql-tag';

import Form from 'react-bootstrap/Form';
import { createClient } from './client';

interface Props {
    id: number;
}

const Rozpis = gql(`
  query Rozpis($id: bigint!) {
    rozpis_by_pk(r_id: $id) {
      ...scheduleFields
      ...scheduleItemFields
    }
  }
`);

export const Nabidka = gql(`
  query Nabidka($id: bigint!) {
    nabidka_by_pk(n_id: $id) {
      ...reservationFields
      ...reservationItemFields
    }
  }
`);

const ToggleVisibleRozpis = gql(`
  mutation setRozpisVisible($id: bigint!, $visible: Boolean!) {
    update_rozpis_by_pk(pk_columns: {r_id: $id}, _set: {r_visible: $visible}) {
      r_id
    }
  }
`);

const ToggleVisibleNabidka = gql(`
  mutation setNabidkaVisible($id: bigint!, $visible: Boolean!) {
    update_nabidka_by_pk(pk_columns: {n_id: $id}, _set: {n_visible: $visible}) {
      n_id
    }
  }
`);


const ScheduleVisible = ({ id }: Props) => {
    const { loading, error, data, refetch } = useQuery(Rozpis, {
        variables: { id },
    });
    const [toggleVisible] = useMutation(ToggleVisibleRozpis, {
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
    const { loading, error, data, refetch } = useQuery(Nabidka, {
        variables: { id },
    });
    const [toggleVisible] = useMutation(ToggleVisibleNabidka, {
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
