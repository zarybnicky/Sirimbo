import { CallToAction } from '@app/branding-olymp/CallToAction';
import { ArticlesDocument } from '@app/graphql/Articles';
import { Heading } from '@app/ui/Heading';
import { Pagination } from '@app/ui/Pagination';
import { ArticleCard } from '@app/ui/cards/ArticleCard';
import { slugify } from '@app/ui/slugify';
import { NextSeo } from 'next-seo';
import type { NextPageWithLayout } from 'pages/_app';
import * as React from 'react';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const [page, setPage] = React.useState(1);
  const [{ data }] = useQuery({query: ArticlesDocument, variables: { first: 12, offset: (page - 1) * 12 }});

  return (
    <>
      <Heading>Aktuálně</Heading>
      <NextSeo title="Články" />

      <div className="col-feature grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mb-6">
        {data?.aktualities?.nodes.map((x) => (
          <ArticleCard
            key={x.id}
            header={x.atJmeno}
            href={`/articles/${x.id}/${slugify(x.atJmeno)}`}
            img={`/galerie/${x.galerieFotoByAtFotoMain?.gfPath || ''}`}
            preview={x.atPreview}
          />
        ))}
      </div>
      <Pagination total={data?.aktualities?.totalCount || 0} limit={12} page={page} setPage={setPage} />

      <CallToAction url="/articles" />
    </>
  );
}

Page.showTopMenu = true;

export default Page;
