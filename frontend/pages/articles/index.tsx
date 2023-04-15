import * as React from 'react';
import { ArticleCard } from 'components/cards/ArticleCard';
import { useArticles } from 'lib/data/use-articles';
import { CallToAction } from 'components/CallToAction';
import Head from 'next/head';
import { Pagination } from 'components/Pagination';
import { Layout } from 'components/layout/Layout';
import { Heading } from 'components/Heading';

export default function ArticleListPage() {
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(1);
  const { articles, count: total } = useArticles(limit, (page - 1) * limit);

  return (
    <>
      <Head>
        <title>Články | TK Olymp</title>
      </Head>
      <Heading color={{ r: 190, g: 10, b: 10, a: 0.5 }} text="Aktuálně" image="" />

      <div className="col-feature grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mb-6">
        {articles.map((x, i) => (
          <ArticleCard key={i} item={x} />
        ))}
      </div>

      <Pagination {...{ total, limit, page, setPage }} />
      <CallToAction />
    </>
  );
}

ArticleListPage.getLayout = (page: React.ReactElement) => (
  <Layout showTopMenu>{page}</Layout>
);
