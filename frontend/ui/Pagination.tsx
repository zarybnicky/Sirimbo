import { cn } from '@/lib/cn';
import { ChevronsLeft, ChevronsRight } from 'lucide-react';
import Link from 'next/link';

type PageItem = number | 'break';

type PaginationProps = {
  total: number;
  limit: number;
  page: number;
} & (
  | {
      href: (page: number) => string;
      onPageChange?: never;
    }
  | {
      href?: never;
      onPageChange: (page: number) => void;
    }
);

export function Pagination({ total, limit, page, href, onPageChange }: PaginationProps) {
  const pageCount = Math.max(1, Math.ceil(total / limit));
  const currentPage = Math.min(Math.max(page, 1), pageCount);

  if (total <= limit) return null;

  const items = getPageItems(currentPage, pageCount);

  const pageButtonClassName =
    'w-10 h-10 bg-accent-3 inline-flex items-center justify-center rounded-full border border-accent-6';
  const arrowButtonClassName =
    'w-6 h-10 inline-flex items-center justify-center rounded-full disabled:opacity-40';

  return (
    <nav aria-label="Stránkování" className="flex flex-wrap gap-1 my-4 text-accent-9">
      {currentPage > 1 &&
        (href ? (
          <Link
            href={href(currentPage - 1)}
            aria-label="Předchozí stránka"
            className={arrowButtonClassName}
          >
            <ChevronsLeft aria-hidden="true" className="size-4" />
          </Link>
        ) : (
          <button
            type="button"
            aria-label="Předchozí stránka"
            className={arrowButtonClassName}
            disabled={currentPage === 1}
            onClick={() => onPageChange(currentPage - 1)}
          >
            <ChevronsLeft aria-hidden="true" className="size-4" />
          </button>
        ))}

      {items.map((item, index) =>
        item === 'break' ? (
          <span key={`break-${index}`} className="flex mx-1 items-center">
            ...
          </span>
        ) : href ? (
          <Link
            key={item}
            href={href(item)}
            aria-current={item === currentPage ? 'page' : undefined}
            className={cn(
              pageButtonClassName,
              item === currentPage && 'text-white !bg-accent-5',
            )}
          >
            {item}
          </Link>
        ) : (
          <button
            key={item}
            type="button"
            aria-current={item === currentPage ? 'page' : undefined}
            className={cn(
              pageButtonClassName,
              item === currentPage && 'text-white !bg-accent-5',
            )}
            onClick={() => onPageChange(item)}
          >
            {item}
          </button>
        ),
      )}

      {currentPage < pageCount &&
        (href ? (
          <Link
            href={href(currentPage + 1)}
            aria-label="Další stránka"
            className={arrowButtonClassName}
          >
            <ChevronsRight aria-hidden="true" className="size-4" />
          </Link>
        ) : (
          <button
            type="button"
            aria-label="Další stránka"
            className={arrowButtonClassName}
            disabled={currentPage === pageCount}
            onClick={() => onPageChange(currentPage + 1)}
          >
            <ChevronsRight aria-hidden="true" className="size-4" />
          </button>
        ))}
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
