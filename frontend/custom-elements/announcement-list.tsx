import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloClient, ApolloProvider, HttpLink, InMemoryCache } from '@apollo/client';
import { useState } from 'react';
import { DateEl } from './date';
import { Pagination } from './pagination';
import { useTypedQuery } from 'lib/zeus/apollo';
import { $, UpozornenisOrderBy, Selector } from 'lib/zeus';
import { scalars } from 'lib/apollo';

const AnnouncementQuery = Selector('Query')({
  upozornenis: [
    {
      first: $('limit', 'Int!'),
      offset: $('offset', 'Int!'),
      orderBy: [UpozornenisOrderBy.UP_TIMESTAMP_ADD_DESC],
    },
    {
      totalCount: true,
      nodes: {
        upId: true,
        upKdo: true,
        upLock: true,
        upNadpis: true,
        upText: true,
        upTimestamp: true,
        upTimestampAdd: true,
        userByUpKdo: {
          uId: true,
          uJmeno: true,
          uPrijmeni: true,
        },
        upozorneniSkupiniesByUpsIdRodic: [{}, {
          nodes: {
            skupinyByUpsIdSkupina: {
              sName: true,
              sDescription: true,
              sColorText: true,
              sColorRgb: true,
            }
          }
        }],
      },
    },
  ],
});

export function AnnouncementList() {
  const [limit] = useState(10);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data } = useTypedQuery(AnnouncementQuery, {
    scalars,
    apolloOptions: {
      variables: { limit, offset },
      onCompleted: (data) => {
        const total = data.upozornenis?.totalCount;
        total && setTotal(total);
      },
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  return <React.Fragment>
    {!data?.upozornenis ? null : data.upozornenis.nodes.map((a) => <React.Fragment>
      <div className="row">
        <div className="col h3">{a.upNadpis}</div>
        <div className="col-12 col-md-4 text-right h6">
          {a.userByUpKdo?.uJmeno} {a.userByUpKdo?.uPrijmeni}{', '}
          <DateEl date={a.upTimestampAdd} />
        </div>
        {a.upozorneniSkupiniesByUpsIdRodic?.nodes?.length <= 0 ? null : <div className="col-12">
          <span className="little">skupiny:&nbsp;</span>
          {a.upozorneniSkupiniesByUpsIdRodic.nodes.map((g) =>
            <div className="box"
              key={g.skupinyByUpsIdSkupina?.sColorRgb}
              title={g.skupinyByUpsIdSkupina?.sName}
              style={{ backgroundColor: g.skupinyByUpsIdSkupina?.sColorRgb }}
            ></div>
          )}
        </div>}
      </div>
      <div style={{ paddingTop: '8px' }} dangerouslySetInnerHTML={{ __html: a.upText }}></div>
      <hr />
    </React.Fragment>)}
    <Pagination {...{ total, limit, setPage }} />
  </React.Fragment>;
}

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export class AnnouncementListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={client}><AnnouncementList /></ApolloProvider>,
      this
    );
  }
}
