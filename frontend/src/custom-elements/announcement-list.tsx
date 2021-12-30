import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { DateEl } from './date';
import { ApolloProvider } from '@apollo/client';
import { createClient } from '../client';
import { Pagination } from './pagination';
import { useTypedQuery } from '../zeus/apollo';
import { $, UpozornenisOrderBy, Selector } from '../zeus';

const AnnouncementQuery = Selector('Query')({
  allUpozornenis: [
    { first: $`limit`, offset: $`offset`, orderBy: [UpozornenisOrderBy.UP_TIMESTAMP_ADD_DESC] },
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
  const [limit, setLimit] = useState(10);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data } = useTypedQuery(AnnouncementQuery, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.allUpozornenis?.totalCount;
      total && setTotal(total);
    },
  });
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  return <React.Fragment>
    {!data?.allUpozornenis ? null : data.allUpozornenis.nodes.map((a) => <React.Fragment>
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

export class AnnouncementListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={createClient()}><AnnouncementList /></ApolloProvider>,
      this
    );
  }
}
