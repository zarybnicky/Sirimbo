import { ChevronsLeft, ChevronsRight } from 'lucide-react';

type PageItem = number | 'break';

export function Pagination({
  total,
  limit,
  page,
  setPage,
}: {
  total: number;
  limit: number;
  page: number;
  setPage: (page: number) => void;
}) {
  const pageCount = Math.max(1, Math.ceil(total / limit));
  const currentPage = Math.min(Math.max(page, 1), pageCount);

  if (total <= limit) return null;

  const items = getPageItems(currentPage, pageCount);

  const pageButtonClassName =
    'w-10 h-10 bg-accent-3 inline-flex items-center justify-center rounded-full border border-accent-6';
  const arrowButtonClassName =
    'w-6 h-10 inline-flex items-center justify-center rounded-full disabled:opacity-40';

  return (
    <nav className="flex flex-wrap gap-1 my-4 text-accent-9">
      <button
        type="button"
        className={arrowButtonClassName}
        disabled={currentPage === 1}
        onClick={() => setPage(currentPage - 1)}
      >
        <ChevronsLeft className="size-4" />
      </button>

      {items.map((item, index) =>
        item === 'break' ? (
          <span key={`break-${index}`} className="flex mx-1 items-center">
            ...
          </span>
        ) : (
          <button
            key={item}
            type="button"
            className={
              item === currentPage
                ? `${pageButtonClassName} text-white !bg-accent-5`
                : pageButtonClassName
            }
            onClick={() => setPage(item)}
          >
            {item}
          </button>
        ),
      )}

      <button
        type="button"
        className={arrowButtonClassName}
        disabled={currentPage === pageCount}
        onClick={() => setPage(currentPage + 1)}
      >
        <ChevronsRight className="size-4" />
      </button>
    </nav>
  );
}

function getPageItems(currentPage: number, pageCount: number): PageItem[] {
  const visible = new Set([1, pageCount]);

  for (let page = currentPage - 1; page <= currentPage + 1; page += 1) {
    if (page > 1 && page < pageCount) {
      visible.add(page);
    }
  }

  const pages = [...visible].toSorted((a, b) => a - b);
  const items: PageItem[] = [];

  for (const page of pages) {
    const previous = items.at(-1);
    if (typeof previous === 'number' && page - previous > 1) {
      items.push('break');
    }
    items.push(page);
  }

  return items;
}
