/* eslint-disable import-x/no-unused-modules */
import { ArticlesDocument } from '@/graphql/Articles';
import { cn } from '@/lib/cn';
import { executeGraphql } from '@/lib/server/graphql';
import { createPublicPageMetadata } from '@/lib/seo';
import { slugify } from '@/lib/slugify';
import { ArticleCard } from '@/ui/ArticleCard';
import { PageHeader } from '@/ui/TitleBar';
import { ChevronsLeft, ChevronsRight } from 'lucide-react';
import type { Metadata } from 'next';
import Link from 'next/link';

const pageSize = 12;
type PageItem = number | 'break';

type ArticlesPageProps = {
  searchParams: Promise<{
    page?: string | string[];
  }>;
};

async function getCurrentPage(searchParams: ArticlesPageProps['searchParams']) {
  const params = await searchParams;
  const rawPage = Array.isArray(params.page) ? params.page[0] : params.page;
  return Math.max(1, Number.parseInt(rawPage ?? '1', 10) || 1);
}

export async function generateMetadata({
  searchParams,
}: ArticlesPageProps): Promise<Metadata> {
  const currentPage = await getCurrentPage(searchParams);

  return createPublicPageMetadata({
    title: currentPage > 1 ? `Články - ${currentPage}. stránka` : 'Články',
    description:
      'Aktuality, články a pozvánky TK Olymp Olomouc ze světa tanečního sportu, soutěží, soustředění, tréninků a klubových akcí.',
    path: currentPage > 1 ? `/clanky?page=${currentPage}` : '/clanky',
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
  const currentPage = await getCurrentPage(searchParams);
  const { aktualities } = await executeGraphql(ArticlesDocument, {
    first: pageSize,
    offset: (currentPage - 1) * pageSize,
    visibleOnly: true,
  });
  const totalPages = Math.max(1, Math.ceil((aktualities?.totalCount ?? 0) / pageSize));

  return (
    <>
      <PageHeader title="Články" />

      <div className="col-feature grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mb-6">
        {aktualities?.nodes?.map((item) => (
          <ArticleCard
            key={item.id}
            href={`/clanky/${item.id}/${slugify(item.atJmeno)}`}
            img={item.titlePhotoUrl ?? undefined}
            header={item.atJmeno}
            preview={item.atPreview}
          />
        ))}
      </div>

      {totalPages > 1 && (
        <nav className="flex flex-wrap gap-1 my-4 text-accent-9">
          {currentPage > 1 && (
            <Link
              href={currentPage <= 2 ? '/clanky' : `/clanky?page=${currentPage - 1}`}
              aria-label="Předchozí stránka"
              className={'w-6 h-10 inline-flex items-center justify-center rounded-full aria-disabled:opacity-40'}
            >
              <ChevronsLeft className="size-4" />
            </Link>
          )}

          {getPageItems(currentPage, totalPages).map((item, index) =>
            item === 'break' ? (
              <span key={`break-${index}`} className="flex mx-1 items-center">
                ...
              </span>
            ) : (
              <Link
                key={item}
                href={item <= 1 ? '/clanky' : `/clanky?page=${item}`}
                aria-current={item === currentPage ? 'page' : undefined}
                className={cn(
                  item === currentPage ? 'text-white !bg-accent-5' : '',
                  'size-10 bg-accent-3 inline-flex items-center justify-center rounded-full border border-accent-6'
                )}
              >
                {item}
              </Link>
            ),
          )}

          {currentPage < totalPages && (
            <Link
              href={`/clanky?page=${currentPage + 1}`}
              aria-label="Další stránka"
              className={'w-6 h-10 inline-flex items-center justify-center rounded-full aria-disabled:opacity-40'}
            >
              <ChevronsRight className="size-4" />
            </Link>
          )}
        </nav>
      )}
    </>
  );
}
