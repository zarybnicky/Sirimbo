import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import { DateEl } from './date';
import * as queries from './queries';
import { ApolloProvider, useQuery } from '@apollo/client';
import { createClient } from './client';
import { Pagination } from './pagination';

export function AnnouncementList() {
    const [limit, setLimit] = useState(10);
    const [offset, setOffset] = useState(0);
    const { data } = useQuery(queries.UpozorneniListDocument, {
        variables: { limit, offset },
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

        <Pagination
            total={data?.upozorneni_aggregate?.aggregate?.count || 0}
            limit={limit} setPage={setPage}
        ></Pagination>
    </React.Fragment>;
}
class AnnouncementListElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={createClient()}><AnnouncementList /></ApolloProvider>,
            this
        );
    }
}
customElements.define('announcement-list', AnnouncementListElement);
