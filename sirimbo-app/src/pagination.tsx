import * as React from 'react';
import ReactPaginate from 'react-paginate';

export const Pagination = ({
    total, limit, setPage,
}: {
    total: number;
    limit: number;
    setPage: (x: { selected: number; }) => void
}) => !total ? null : <div className="navigation">
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
</div>;
