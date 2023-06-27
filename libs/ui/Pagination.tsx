import * as React from 'react';
import ReactPaginate from 'react-paginate';
import { ChevronsLeft, ChevronsRight } from 'lucide-react';

export const Pagination = ({
  total,
  limit,
  page,
  setPage,
}: {
  total: number;
  limit: number;
  page: number;
  setPage: (page: number) => void;
}) => {
  return (
    <ReactPaginate
      breakLabel="..."
      containerClassName="flex flex-wrap gap-1 my-4 text-primary"
      breakClassName="flex mx-1 items-center"
      previousLinkClassName="w-6 h-10 inline-flex items-center justify-center rounded-full"
      nextLinkClassName="w-6 h-10 inline-flex items-center justify-center rounded-full"
      pageLinkClassName="w-10 h-10 bg-white inline-flex items-center justify-center rounded-full border border-red-500"
      activeLinkClassName="text-white !bg-primary"
      nextLabel={<ChevronsRight className="w-4 h-4" />}
      previousLabel={<ChevronsLeft className="w-4 h-4" />}
      onPageChange={({ selected }) => setPage(selected + 1)}
      forcePage={Math.max(page - 1, 0)}
      pageCount={Math.ceil(total / limit)}
      marginPagesDisplayed={1}
      pageRangeDisplayed={2}
    />
  );
};
