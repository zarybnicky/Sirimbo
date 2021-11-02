import * as React from 'react';
import { useState } from 'react';
import * as ReactDOM from 'react-dom';
import ReactPaginate from 'react-paginate';
import { DateEl } from './date';
import * as queries from './queries';
import { ApolloProvider, useQuery } from '@apollo/client';
import { createClient } from './client';

export function Announcements() {
    const [limit, setLimit] = useState(10);
    const [offset, setOffset] = useState(0);
    const { loading, error, data } = useQuery(queries.UpozorneniListDocument, {
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

        {(data?.upozorneni_aggregate?.aggregate?.count || 0) <= 0 ? null :
            <div className="navigation">
                <ReactPaginate
                    previousLabel={'<'}
                    nextLabel={'>'}
                    breakLabel={'...'}
                    pageCount={Math.ceil((data?.upozorneni_aggregate?.aggregate?.count || 0) / limit)}
                    marginPagesDisplayed={2}
                    pageRangeDisplayed={5}
                    onPageChange={setPage}

                    containerClassName="pagination"
                    breakClassName="page-item"
                    breakLinkClassName="page-link"
                    pageClassName="page-item"
                    previousClassName="page-item"
                    nextClassName="page-item"
                    pageLinkClassName="page-link"
                    previousLinkClassName="page-link"
                    nextLinkClassName="page-link"
                    activeClassName="active"
                />
            </div>
        }
    </React.Fragment >;
}
class AnnouncementsElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={createClient()}><Announcements /></ApolloProvider>,
            this
        );
    }
}
customElements.define('announcement-list', AnnouncementsElement);
