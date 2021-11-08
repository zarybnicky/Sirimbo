import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { Pagination } from './pagination';
import { DateRange } from './date';
import { gql } from 'graphql-tag';
import { createClient } from './client';
import { Dropdown } from './dropdown';

export const NabidkaAdminQuery = gql(`
query ReservationAdminList($offset: Int, $limit: Int) {
  nabidka: nabidka_admin(limit: $limit, offset: $offset, order_by: {n_od: desc}) {
    n_visible
    n_trener
    n_timestamp
    n_pocet_hod
    n_od
    n_max_pocet_hod
    n_lock
    n_id
    n_do
    user {
      u_jmeno
      u_prijmeni
      u_id
    }
    nabidka_items {
      ni_lock
      ni_partner
      ni_pocet_hod
      pary {
        user {
          u_id
          u_jmeno
          u_prijmeni
        }
      }
    }
  }
  aggregate: nabidka_admin_aggregate {
    aggregate {
      count
    }
  }
}`);

const ToggleVisibleNabidka = gql(`
  mutation setNabidkaVisible($id: bigint!, $visible: Boolean!) {
    update_nabidka_admin(where: {n_id: {_eq: $id}}, _set: {n_visible: $visible}) {
      affected_rows
    }
  }
`);

export function ReservationAdminList() {
    const [limit, setLimit] = useState(30);
    const [offset, setOffset] = useState(0);
    const [total, setTotal] = useState(0);
    const { data, refetch } = useQuery(NabidkaAdminQuery, {
        variables: { limit, offset },
        onCompleted: (data) => {
            const total = data.aggregate?.aggregate?.count;
            total && setTotal(total);
        },
    });
    const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
    const [toggleVisible] = useMutation(ToggleVisibleNabidka, {
        onCompleted: () => refetch(),
    });

    const list = !data?.nabidka.length ? null : <table>
        <thead>
            <tr>
                <th>Trenér</th>
                <th>Datum</th>
                <th>Viditelný</th>
            </tr>
        </thead>
        <tbody>
            {data!.nabidka.map((a) => <tr key={a.n_id}>
                <td>
                    <Dropdown links={{
                        [`/admin/nabidka/edit/${a.n_id}`]: "Upravit",
                        [`/admin/nabidka/detail/${a.n_id}`]: "Upravit lekce",
                        [`/admin/nabidka/duplicate/${a.n_id}`]: "Duplikovat",
                        [`/admin/nabidka/remove/${a.n_id}`]: "Odstranit",
                    }} />
                    {a?.user?.u_jmeno} {a?.user?.u_prijmeni}
                </td>
                <td><DateRange from={a.n_od} to={a.n_do} /></td>
                <td>
                    <Form.Check checked={a.n_visible || false} onChange={() => toggleVisible({
                        variables: { id: a.n_id, visible: !a.n_visible },
                    })} />
                </td>
            </tr>)}
        </tbody>
    </table >;

    return <React.Fragment>
        <a href="/admin/nabidka/add" className="btn btn-primary">Nová nabídka</a>
        {list}
        <Pagination {...{ total, limit, setPage }} />
    </React.Fragment>;
}

export class ReservationAdminListElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={createClient()}><ReservationAdminList /></ApolloProvider>,
            this
        );
    }
}
