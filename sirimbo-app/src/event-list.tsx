import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { createClient } from './client';
import { Pagination } from './pagination';
import { DateRange } from './date';
import { gql } from 'graphql-tag';

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
            {data!.akce.map((a) => <tr>
                <td>
                    <div className="btn-group">
                        <button type="button" className="btn btn-xs pt-0" data-toggle="dropdown">
                            <img alt="Upravit" width="14" src="/style/icon-gear.png" />
                        </button>
                        <div className="dropdown-menu">
                            <a className="dropdown-item" href={`/admin/akce/edit/${a.a_id}`}>Upravit</a>
                            <a className="dropdown-item" href={`/admin/akce/detail/${a.a_id}`}>Upravit účastníky</a>
                            <a className="dropdown-item" href={`/admin/akce/dokumenty/${a.a_id}`}>Upravit dokumenty</a>
                            <a className="dropdown-item" href={`/admin/akce/remove/${a.a_id}`}>Odstranit</a>
                        </div>
                    </div>
                    {a.a_jmeno}
                </td>
                <td><DateRange from={a.a_od} to={a.a_do} /></td>
                <td>{a.aggregate!.aggregate!.count}/{a.a_kapacita}</td>
                <td>
                    <Form.Check checked={a.a_visible} onChange={() => toggleVisible({ variables: { id: a.a_id, visible: !a.a_visible } })} />
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
