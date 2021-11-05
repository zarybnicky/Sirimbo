import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { createClient } from './client';
import { Pagination } from './pagination';
import { DateRange } from './date';
import { gql } from 'graphql-tag';
import { Dropdown } from './dropdown';

export const AkceList = gql(`
query AkceList($offset: Int, $limit: Int) {
  akce(limit: $limit, offset: $offset, order_by: {a_od: desc}) {
    ...eventFields
    aggregate: akce_items_aggregate {
      aggregate {
        count
      }
    }
  }
  aggregate: akce_aggregate {
    aggregate {
      count
    }
  }
}`);

const ToggleVisible = gql(`
  mutation setNabidkaVisible($id: bigint!, $visible: Boolean!) {
    update_nabidka_by_pk(pk_columns: {n_id: $id}, _set: {n_visible: $visible}) {
      n_id
    }
  }
`);

export function EventList() {
    const [limit, setLimit] = useState(30);
    const [offset, setOffset] = useState(0);
    const { data, refetch } = useQuery(AkceList, {
        variables: { limit, offset },
    });
    const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
    const [toggleVisible] = useMutation(ToggleVisible, {
        onCompleted: () => refetch(),
    });

    const list = !data?.akce.length ? null : <table>
        <thead>
            <tr>
                <th>Jméno</th>
                <th>Datum</th>
                <th>Kapacita</th>
                <th>Viditelný</th>
            </tr>
        </thead>
        <tbody>
            {data!.akce.map((a) => <tr key={a.a_id}>
                <td>
                    <Dropdown links={{
                        [`/admin/akce/edit/${a.a_id}`]: "Upravit",
                        [`/admin/akce/detail/${a.a_id}`]: "Upravit účastníky",
                        [`/admin/akce/dokumenty/${a.a_id}`]: "Upravit dokumenty",
                        [`/admin/akce/remove/${a.a_id}`]: "Odstranit",
                    }} />
                    {a.a_jmeno}
                </td>
                <td><DateRange from={a.a_od} to={a.a_do} /></td>
                <td>{a.aggregate!.aggregate!.count}/{a.a_kapacita}</td>
                <td>
                    <Form.Check checked={a.a_visible} onChange={() => toggleVisible({
                        variables: { id: a.a_id, visible: !a.a_visible },
                    })} />
                </td>
            </tr>)}
        </tbody>
    </table>;

    return <React.Fragment>
        <a href="/admin/akce/add" className="btn btn-primary">Přidat</a>
        {list}
        <Pagination
            total={data?.aggregate?.aggregate?.count || 0}
            limit={limit} setPage={setPage}
        ></Pagination>
    </React.Fragment>;
}
class EventListElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={createClient()}><EventList /></ApolloProvider>,
            this
        );
    }
}
customElements.define('event-list', EventListElement);
