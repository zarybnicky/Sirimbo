import * as React from 'react';
import ReactPaginate from 'react-paginate';
import { ChevronLeft, ChevronRight } from 'react-feather';

export const Pagination = ({ total, limit, page, setPage }: {
  total: number;
  limit: number;
  page: number;
  setPage: (page: number) => void;
}) => {
  const [isMobile, setIsMobile] = React.useState(false);
  React.useEffect(() => {
    if (typeof window === 'undefined') return;
    const updateDetailView = () => setIsMobile(!window.matchMedia("(min-width: 768px)").matches);
    window.addEventListener('resize', updateDetailView);
    return () => window.removeEventListener('resize', updateDetailView);
  }, []);

  return <ReactPaginate
    breakLabel="..."
    containerClassName="flex flex-wrap gap-2 mb-4"
    breakClassName="flex mx-2 items-center"
    previousLinkClassName="button button-icon"
    nextLinkClassName="button button-icon"
    pageLinkClassName="button button-icon"
    activeLinkClassName="button button-icon button-red"
    nextLabel={<ChevronRight className="w-4 h-4" />}
    previousLabel={<ChevronLeft className="w-4 h-4" />}
    onPageChange={({ selected }) => setPage(selected)}
    forcePage={Math.max(page - 1, 0)}
    pageCount={Math.ceil(total / limit)}
    marginPagesDisplayed={isMobile ? 1 : 2}
    pageRangeDisplayed={isMobile ? 2 : 3}
  />;
};
