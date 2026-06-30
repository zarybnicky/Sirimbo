import { ArticleForm } from '@/ui/forms/ArticleForm';
import { ArticleList } from '@/ui/lists/ArticleList';
import { Layout } from '@/ui/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';
import { z } from 'zod';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { useQuery } from 'urql';
import { ArticleDocument } from '@/graphql/Articles';

const QueryParams = z.object({
  id: zRouterId,
});

export default function ArticlePage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  const [{ data }] = useQuery({
    query: ArticleDocument,
    variables: { id },
    pause: !id,
  });
  const title = data?.aktuality?.atJmeno || 'Aktuality';

  return (
    <Layout requireTrainer>
      <NextSeo title={title} />
      <WithSidebar sidebar={<ArticleList />}>
        <ArticleForm id={id} />
      </WithSidebar>
    </Layout>
  );
}
