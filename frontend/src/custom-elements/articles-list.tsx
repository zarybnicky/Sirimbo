import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { ApolloProvider, useQuery } from '@apollo/client';
import { Pagination } from './pagination';
import { DateEl } from './date';
import { createClient, UserQuery } from '../client';
import { Dropdown } from './dropdown';
import { $, AktualitiesOrderBy, Selector } from '../zeus';
import { useTypedQuery } from '../zeus/apollo';

export const ArticlesAdminQuery = Selector('Query')({
  allAktualities: [
    { first: $`limit`, offset: $`offset`, orderBy: [AktualitiesOrderBy.AT_TIMESTAMP_ADD_DESC] },
    {
      nodes: {
        atFoto: true,
        atFotoMain: true,
        atId: true,
        atJmeno: true,
        atKdo: true,
        atPreview: true,
        atText: true,
        atTimestampAdd: true,
        atTimestamp: true,
      },
      totalCount: true,
    },
  ],
});

export function ArticleAdminList() {
  const [limit, setLimit] = useState(30);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data: user } = useQuery(UserQuery);
  const { data } = useTypedQuery(ArticlesAdminQuery, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.allAktualities?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);

  const list = (!user || !data?.allAktualities?.nodes.length) ? null : <table>
    <thead>
      <tr><th>Jméno</th><th>Přidáno</th></tr>
    </thead>
    <tbody>
      {data!.allAktualities?.nodes.filter(
        a => 16 <= (user.getCurrentUser?.permissionByUGroup?.peAktuality || 0)
          || a.atKdo == user.getCurrentUser?.uId
      ).map((a) => <tr key={a.atId}>
        <td>
          <Dropdown links={{
            [`/admin/aktuality/edit/${a.atId}`]: "Upravit",
            [`/admin/aktuality/foto/${a.atId}`]: "Upravit fotky",
            [`/admin/aktuality/remove/${a.atId}`]: "Odstranit",
          }} />
          {a.atJmeno}
        </td>
        <td><DateEl date={a.atTimestampAdd} /></td>
      </tr>)}
    </tbody>
  </table >;

  return <React.Fragment>
    <a href="/admin/aktuality/add" className="btn btn-primary">Nový článek</a>
    {list}
    <Pagination {...{ total, limit, setPage }} />
  </React.Fragment>;
}

export class ArticleAdminListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={createClient()}><ArticleAdminList /></ApolloProvider>,
      this
    );
  }
}
