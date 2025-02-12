import { ArticleForm } from '@/ui/forms/ArticleForm';
import { ArticleList } from '@/ui/lists/ArticleList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';
import { z } from 'zod';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';

const QueryParams = z.object({
  id: zRouterId,
});

export default function ArticlePage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query;
  return (
    <Layout requireTrainer>
      <NextSeo title="Aktuality" />
      <WithSidebar sidebar={<ArticleList />}>
        <ArticleForm id={id} />
      </WithSidebar>
    </Layout>
  );
}
