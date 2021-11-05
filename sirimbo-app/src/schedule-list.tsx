import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { Pagination } from './pagination';
import { DateEl } from './date';
import { gql } from 'graphql-tag';
import { createClient } from './client';
import { Dropdown } from './dropdown';

export const RozpisAdminQuery = gql(`
query ScheduleAdminList($offset: Int, $limit: Int) {
  rozpis: rozpis_admin(limit: $limit, offset: $offset, order_by: {r_datum: desc}) {
    r_datum
    r_id
    r_kde
    r_lock
    r_timestamp
    r_trener
    r_visible
    user {
      u_jmeno
      u_prijmeni
      u_id
    }
    rozpis_items {
      ri_od
      ri_do
      ri_id
      ri_partner
    }
  }
  aggregate: rozpis_admin_aggregate {
    aggregate {
      count
    }
  }
}`);

const ToggleVisibleRozpis = gql(`
  mutation setRozpisVisible($id: bigint!, $visible: Boolean!) {
    update_rozpis_by_pk(pk_columns: {r_id: $id}, _set: {r_visible: $visible}) {
      r_id
    }
  }
`);

export function RozpisAdminList() {
    const [limit, setLimit] = useState(30);
    const [offset, setOffset] = useState(0);
    const [total, setTotal] = useState(0);
    const { data, refetch } = useQuery(RozpisAdminQuery, {
        variables: { limit, offset },
        onCompleted: (data) => {
            const total = data.aggregate?.aggregate?.count;
            total && setTotal(total);
        },
    });
    const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
    const [toggleVisible] = useMutation(ToggleVisibleRozpis, {
        onCompleted: () => refetch(),
    });

    const list = !data?.rozpis.length ? null : <table>
        <thead>
            <tr>
                <th>Trenér</th>
                <th>Datum</th>
                <th>Místo</th>
                <th>Viditelný</th>
            </tr>
        </thead>
        <tbody>
            {data!.rozpis.map((a) => <tr key={a.r_id}>
                <td>
                    <Dropdown links={{
                        [`/admin/rozpis/edit/${a.r_id}`]: "Upravit",
                        [`/admin/rozpis/detail/${a.r_id}`]: "Upravit lekce",
                        [`/admin/rozpis/duplicate/${a.r_id}`]: "Duplikovat",
                        [`/admin/rozpis/remove/${a.r_id}`]: "Odstranit",
                    }} />
                    {a?.user?.u_jmeno} {a?.user?.u_prijmeni}
                </td>
                <td><DateEl date={a.r_datum} /></td>
                <td>{a.r_kde}</td>
                <td>
                    <Form.Check checked={a.r_visible || false} onChange={() => toggleVisible({
                        variables: { id: a.r_id, visible: !a.r_visible },
                    })} />
                </td>
            </tr>)}
        </tbody>
    </table >;

    return <React.Fragment>
        <a href="/admin/rozpis/add" className="btn btn-primary">Nový rozpis</a>
        {list}
        <Pagination {...{ total, limit, setPage }} />
    </React.Fragment>;
}
class RozpisAdminListElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={createClient()}><RozpisAdminList /></ApolloProvider>,
            this
        );
    }
}
customElements.define('rozpis-admin-list', RozpisAdminListElement);
