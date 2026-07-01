/* eslint-disable import-x/no-unused-modules */
import { ArticlesDocument } from '@/graphql/Articles';
import { executeGraphql } from '@/lib/server/graphql';
import { slugify } from '@/lib/slugify';
import { ArticleCard } from '@/ui/ArticleCard';
import { PageHeader } from '@/ui/TitleBar';
import { ChevronsLeft, ChevronsRight } from 'lucide-react';
import type { Metadata } from 'next';
import Link from 'next/link';
import { route } from 'nextjs-routes';

const pageSize = 12;
type PageItem = number | 'break';

export const metadata: Metadata = {
  title: 'Články',
  alternates: {
    canonical: '/clanky',
  },
};

type ArticlesPageProps = {
  searchParams: Promise<{
    page?: string | string[];
  }>;
};

function pageHref(page: number) {
  return route({
    pathname: '/clanky',
    query: page <= 1 ? {} : { page: String(page) },
  });
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

export default async function ArticlesPage({ searchParams }: ArticlesPageProps) {
  const params = await searchParams;
  const rawPage = Array.isArray(params.page) ? params.page[0] : params.page;
  const currentPage = Math.max(1, Number.parseInt(rawPage ?? '1', 10) || 1);
  const offset = (currentPage - 1) * pageSize;
  const data = await executeGraphql(ArticlesDocument, {
    first: pageSize,
    offset,
    visibleOnly: true,
  });
  const articles = data.aktualities?.nodes ?? [];
  const totalCount = data.aktualities?.totalCount ?? 0;
  const totalPages = Math.max(1, Math.ceil(totalCount / pageSize));
  const pageItems = getPageItems(currentPage, totalPages);
  const pageButtonClassName =
    'w-10 h-10 bg-accent-3 inline-flex items-center justify-center rounded-full border border-accent-6';
  const arrowButtonClassName =
    'w-6 h-10 inline-flex items-center justify-center rounded-full aria-disabled:opacity-40';

  return (
    <>
      <PageHeader title="Články" />

      {articles.length > 0 ? (
        <div className="col-feature grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mb-6">
          {articles.map((item) => (
            <ArticleCard
              key={item.id}
              href={route({
                pathname: '/clanky/[id]/[[...slug]]',
                query: {
                  id: item.id,
                  slug: [slugify(item.atJmeno)],
                },
              })}
              img={item.titlePhotoUrl ?? undefined}
              header={item.atJmeno}
              preview={item.atPreview}
            />
          ))}
        </div>
      ) : (
        <p className="text-neutral-11">Zatím tu nejsou žádné články.</p>
      )}

      {totalPages > 1 && (
        <nav className="flex flex-wrap gap-1 my-4 text-accent-9">
          {currentPage > 1 ? (
            <Link
              href={pageHref(currentPage - 1)}
              aria-label="Předchozí stránka"
              className={arrowButtonClassName}
            >
              <ChevronsLeft className="size-4" />
            </Link>
          ) : (
            <span aria-disabled className={arrowButtonClassName}>
              <ChevronsLeft className="size-4" />
            </span>
          )}

          {pageItems.map((item, index) =>
            item === 'break' ? (
              <span key={`break-${index}`} className="flex mx-1 items-center">
                ...
              </span>
            ) : (
              <Link
                key={item}
                href={pageHref(item)}
                aria-current={item === currentPage ? 'page' : undefined}
                className={
                  item === currentPage
                    ? `${pageButtonClassName} text-white !bg-accent-5`
                    : pageButtonClassName
                }
              >
                {item}
              </Link>
            ),
          )}

          {currentPage < totalPages ? (
            <Link
              href={pageHref(currentPage + 1)}
              aria-label="Další stránka"
              className={arrowButtonClassName}
            >
              <ChevronsRight className="size-4" />
            </Link>
          ) : (
            <span aria-disabled className={arrowButtonClassName}>
              <ChevronsRight className="size-4" />
            </span>
          )}
        </nav>
      )}
    </>
  );
}
