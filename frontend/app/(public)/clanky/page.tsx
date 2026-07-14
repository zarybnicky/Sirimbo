/* eslint-disable import-x/no-unused-modules */
import { ArticlesDocument } from '@/graphql/Articles';
import { executeGraphql } from '@/lib/server/graphql';
import { publicPageMetadata } from '@/lib/server/seo';
import { getRequestTenant } from '@/tenant/server';
import { slugify } from '@/lib/slugify';
import { ArticleCard } from '@/ui/ArticleCard';
import { Pagination } from '@/ui/Pagination';
import { PageHeader } from '@/ui/TitleBar';
import type { Metadata } from 'next';

const pageSize = 12;

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

  return publicPageMetadata({
    title: currentPage > 1 ? `Články - ${currentPage}. stránka` : 'Články',
    description:
      'Aktuality, články a pozvánky TK Olymp Olomouc ze světa tanečního sportu, soutěží, soustředění, tréninků a klubových akcí.',
    path: currentPage > 1 ? `/clanky?page=${currentPage}` : '/clanky',
  });
}

export default async function ArticlesPage({ searchParams }: ArticlesPageProps) {
  const currentPage = await getCurrentPage(searchParams);
  const [{ aktualities }, tenant] = await Promise.all([
    executeGraphql(ArticlesDocument, {
      first: pageSize,
      offset: (currentPage - 1) * pageSize,
      visibleOnly: true,
    }),
    getRequestTenant(),
  ]);
  return (
    <>
      <PageHeader title="Články" />

      <div className="col-feature grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mb-6">
        {aktualities?.nodes?.map((item) => (
          <ArticleCard
            key={item.id}
            href={`/clanky/${item.id}/${slugify(item.atJmeno)}`}
            img={item.titlePhotoUrl}
            header={item.atJmeno}
            preview={item.atPreview}
            sizes="(min-width: 1024px) 300px, (min-width: 960px) 440px, calc(50vw - 1.5rem)"
            fallbackImage={tenant.config.publicSite?.image.url}
          />
        ))}
      </div>

      <Pagination
        total={aktualities?.totalCount ?? 0}
        limit={pageSize}
        page={currentPage}
        href={(page) => (page <= 1 ? '/clanky' : `/clanky?page=${page}`)}
      />
    </>
  );
}
