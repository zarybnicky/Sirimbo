import * as React from 'react';
import { ArticleCard } from 'components/cards/ArticleCard';
import { useArticles } from 'lib/data/use-articles';
import { CallToAction } from 'components/CallToAction';
import { Pagination } from 'components/Pagination';
import { Heading } from 'components/Heading';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const [limit] = React.useState(12);
  const [page, setPage] = React.useState(1);
  const { articles, count: total } = useArticles(limit, (page - 1) * limit);

  return (
    <>
      <Heading>Aktuálně</Heading>

      <div className="col-feature grid place-items-stretch gap-4 grid-cols-2 lg:grid-cols-3 mb-6">
        {articles.map((x) => (
          <ArticleCard key={x.id} item={x} />
        ))}
      </div>

      <Pagination {...{ total, limit, page, setPage }} />
      <CallToAction />
    </>
  );
}

Page.staticTitle = "Články";
Page.showTopMenu = true;

export default Page;
