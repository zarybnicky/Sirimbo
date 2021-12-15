import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { DateEl } from './date';
import { ApolloProvider, useQuery } from '@apollo/client';
import { createClient } from './client';
import { Pagination } from './pagination';
import { gql } from 'graphql-tag';

export const UpozorneniList = gql(`
query UpozorneniList($offset: Int, $limit: Int) {
  upozorneni(limit: $limit, offset: $offset, order_by: {up_timestamp_add: desc}) {
    up_id
    up_kdo
    up_lock
    up_nadpis
    up_text
    up_timestamp
    up_timestamp_add
    user {
      u_id
      u_jmeno
      u_prijmeni
    }
    upozorneni_skupinies {
      skupiny {
        s_name
        s_description
        s_color_text
        s_color_rgb
      }
    }
  }
  aggregate: upozorneni_aggregate {
    aggregate {
      count
    }
  }
}`);

export function AnnouncementList() {
    const [limit, setLimit] = useState(10);
    const [offset, setOffset] = useState(0);
    const [total, setTotal] = useState(0);
    const { data } = useQuery(UpozorneniList, {
        variables: { limit, offset },
        onCompleted: (data) => {
            const total = data.aggregate?.aggregate?.count;
            total && setTotal(total);
        },
    });
    const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
    return <React.Fragment>
        {!data?.upozorneni ? null : data.upozorneni.map((a) => <React.Fragment>
            <div className="row">
                <div className="col h3">{a.up_nadpis}</div>
                <div className="col-12 col-md-4 text-right h6">
                    {a.user.u_jmeno} {a.user.u_prijmeni}{', '}
                    <DateEl date={a.up_timestamp_add} />
                </div>
                {a.upozorneni_skupinies.length <= 0 ? null : <div className="col-12">
                    <span className="little">skupiny:&nbsp;</span>
                    {a.upozorneni_skupinies.map((g) =>
                        <div className="box"
                            key={g.skupiny.s_color_rgb}
                            title={g.skupiny.s_name}
                            style={{ backgroundColor: g.skupiny.s_color_rgb }}
                        ></div>
                    )}
                </div>}
            </div>
            <div style={{ paddingTop: '8px' }} dangerouslySetInnerHTML={{ __html: a.up_text }}></div>
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
