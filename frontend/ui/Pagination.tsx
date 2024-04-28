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
  if (total < limit) return;
  return (
    <ReactPaginate
      breakLabel="..."
      containerClassName="flex flex-wrap gap-1 my-4 text-primary"
      breakClassName="flex mx-1 items-center"
      previousLinkClassName="w-6 h-10 inline-flex items-center justify-center rounded-full"
      nextLinkClassName="w-6 h-10 inline-flex items-center justify-center rounded-full"
      pageLinkClassName="w-10 h-10 bg-accent-3 inline-flex items-center justify-center rounded-full border border-accent-6"
      activeLinkClassName="text-white !bg-accent-5"
      nextLabel={<ChevronsRight className="size-4" />}
      previousLabel={<ChevronsLeft className="size-4" />}
      onPageChange={({ selected }) => setPage(selected + 1)}
      forcePage={Math.max(page - 1, 0)}
      pageCount={Math.max(1, Math.ceil(total / limit))}
      marginPagesDisplayed={1}
      pageRangeDisplayed={2}
    />
  );
};
