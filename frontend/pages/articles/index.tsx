import * as React from 'react';
import { ArticleCard } from 'components/cards/ArticleCard';
import { useArticles } from 'lib/data/use-articles';
import { CallToAction } from 'components/CallToAction';
import Head from 'next/head';
import { Pagination } from 'components/Pagination';

export const ArticlesPage = ({ }) => {
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(1);
  const { articles, count: total } = useArticles(limit, (page - 1) * limit);

  return <>
    <Head>
      <title>Články | TK Olymp</title>
    </Head>
    <div className="container mx-auto max-w-5xl" style={{ margin: '4rem auto 3rem' }}>
      <h3 className="text-xl font-bold mb-2">Aktuálně</h3>
      <div className="grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mb-6">
        {articles.map((x, i) => <ArticleCard key={i} item={x} />)}
      </div>

      <Pagination {...{ total, limit, page, setPage }} />
    </div>
    <CallToAction />
  </>;
};

export default ArticlesPage;
