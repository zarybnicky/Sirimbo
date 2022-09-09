import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { useState } from 'react';
import { Pagination } from './pagination';
import { DateEl } from './date';
import { UserQuery } from 'lib/data/use-auth';
import { Dropdown } from './dropdown';
import { $, AktualitiesOrderBy, Selector } from 'lib/zeus';
import { useTypedQuery } from 'lib/zeus/apollo';
import { scalars } from 'lib/apollo';

export const ArticlesAdminQuery = Selector('Query')({
  aktualities: [
    {
      first: $('limit', 'Int!'),
      offset: $('offset', 'Int!'),
      orderBy: [AktualitiesOrderBy.AT_TIMESTAMP_ADD_DESC]
    },
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
  const [limit] = useState(30);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data: user } = useTypedQuery(UserQuery);
  const { data } = useTypedQuery(ArticlesAdminQuery, {
    scalars,
    apolloOptions: {
      variables: { limit, offset },
      onCompleted: (data) => {
        const total = data.aktualities?.totalCount;
        total && setTotal(total);
      },
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);

  const list = (!user || !data?.aktualities?.nodes.length) ? null : <table>
    <thead>
      <tr><th>Jméno</th><th>Přidáno</th></tr>
    </thead>
    <tbody>
      {data!.aktualities?.nodes.filter(
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
        <td>{a.atTimestampAdd && <DateEl date={a.atTimestampAdd} />}</td>
      </tr>)}
    </tbody>
  </table >;

  return <React.Fragment>
    <a href="/admin/aktuality/add" className="btn btn-primary">Nový článek</a>
    {list}
    <Pagination {...{ total, limit, setPage }} />
  </React.Fragment>;
}

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export class ArticleAdminListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={client}><ArticleAdminList /></ApolloProvider>,
      this
    );
  }
}
