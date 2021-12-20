import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { DateEl } from './date';
import { ApolloProvider, useQuery } from '@apollo/client';
import { createClient } from '../client';
import { Pagination } from './pagination';
import { gql } from 'graphql-tag';

export const UpozorneniList = gql(`
query UpozorneniList($offset: Int, $limit: Int) {
  allUpozornenis(first: $limit, offset: $offset, orderBy: UP_TIMESTAMP_ADD_DESC) {
    nodes {
      upId
      upKdo
      upLock
      upNadpis
      upText
      upTimestamp
      upTimestampAdd
      userByUpKdo {
        uId
        uJmeno
        uPrijmeni
      }
      upozorneniSkupiniesByUpsIdRodic {
        nodes {
          skupinyByUpsIdSkupina {
            sName
            sDescription
            sColorText
            sColorRgb
          }
        }
      }
    }
    totalCount
  }
}`);

export function AnnouncementList() {
  const [limit, setLimit] = useState(10);
  const [offset, setOffset] = useState(0);
  const [total, setTotal] = useState(0);
  const { data } = useQuery(UpozorneniList, {
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
