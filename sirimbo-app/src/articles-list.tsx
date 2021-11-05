import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { ApolloProvider, useQuery } from '@apollo/client';
import { Pagination } from './pagination';
import { DateEl } from './date';
import { gql } from 'graphql-tag';
import { createClient } from './client';
import { Dropdown } from './dropdown';

export const ArticlesAdminQuery = gql(`
query ArticlesAdminList($offset: Int, $limit: Int) {
  aktuality: aktuality_admin(limit: $limit, offset: $offset, order_by: {at_timestamp_add: desc}) {
    at_foto
    at_foto_main
    at_id
    at_jmeno
    at_kat
    at_kdo
    at_preview
    at_text
    at_timestamp_add
    at_timestamp
  }
  aggregate: aktuality_admin_aggregate {
    aggregate {
      count
    }
  }
}`);

export function ArticleAdminList() {
    const [limit, setLimit] = useState(30);
    const [offset, setOffset] = useState(0);
    const [total, setTotal] = useState(0);
    const { data } = useQuery(ArticlesAdminQuery, {
        variables: { limit, offset },
        onCompleted: (data) => {
            const total = data.aggregate?.aggregate?.count;
            total && setTotal(total);
        },
    });
    const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);

    const list = !data?.aktuality.length ? null : <table>
        <thead>
            <tr><th>Jméno</th><th>Přidáno</th></tr>
        </thead>
        <tbody>
            {data!.aktuality.map((a) => <tr key={a.at_id}>
                <td>
                    <Dropdown links={{
                        [`/admin/aktuality/edit/${a.at_id}`]: "Upravit",
                        [`/admin/aktuality/foto/${a.at_id}`]: "Upravit fotky",
                        [`/admin/aktuality/remove/${a.at_id}`]: "Odstranit",
                    }} />
                    {a.at_jmeno}
                </td>
                <td><DateEl date={a.at_timestamp_add} /></td>
            </tr>)}
        </tbody>
    </table >;

    return <React.Fragment>
        <a href="/admin/aktuality/add" className="btn btn-primary">Nový článek</a>
        {list}
        <Pagination {...{ total, limit, setPage }} />
    </React.Fragment>;
}
class ArticleAdminListElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={createClient()}><ArticleAdminList /></ApolloProvider>,
            this
        );
    }
}
customElements.define('article-admin-list', ArticleAdminListElement);
