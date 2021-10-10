import * as React from 'react';
import { useState, useEffect } from 'react';
import * as ReactDOM from 'react-dom';
import ReactPaginate from 'react-paginate';
import { DateEl } from './date';
import { Service, PagedResponse_, AnnouncementResponse } from './schema';

export function Announcements() {
    const [limit, setLimit] = useState(10);
    const [offset, setOffset] = useState(0);
    const [data, setData] = useState<AnnouncementResponse[]>([]);
    const [total, setTotal] = useState(0);

    const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);

    useEffect(() => {
        (async () => {
            const response: PagedResponse_ = await Service.getAnnouncements(offset, limit);
            setData(response.items);
            setTotal(response.total);
        })();
    }, [limit, offset]);

    return <React.Fragment>
        {data.map(({ announcement: a, announcementId: aid, user: u, groups: gs }) => <React.Fragment>
            <div key={aid} className="row">
                <div className="col h3">{a.announcementTitle}</div>
                <div className="col-12 col-md-4 text-right h6">
                    {u.userName} {u.userSurname}{', '}
                    <DateEl date={a.announcementCreatedAt} />
                </div>
                {gs.length <= 0 ? null : <div className="col-12">
                    <span className="little">skupiny:&nbsp;</span>
                    {gs.map((g) =>
                        <div className="box"
                            key={g.announcementGroupColor}
                            title="{g.announcementGroupTitle}"
                            style={{ backgroundColor: g.announcementGroupColor }}
                        ></div>
                    )}
                </div>}
            </div>
            <div style={{ paddingTop: '8px' }} dangerouslySetInnerHTML={{ __html: a.announcementText }}></div>
            <hr />
        </React.Fragment>)}

        {total <= 0 ? null :
            <div className="navigation">
                <ReactPaginate
                    previousLabel={'<'}
                    nextLabel={'>'}
                    breakLabel={'...'}
                    pageCount={Math.ceil(total / limit)}
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
            </div>}
    </React.Fragment>;
}
class AnnouncementsElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<Announcements />, this);
    }
}
customElements.define('announcement-list', AnnouncementsElement);
