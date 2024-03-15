import { ArticleForm } from '@app/ui/ArticleForm';
import { ArticleList } from '@app/ui/ArticleList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';
import { z } from 'zod';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';

const QueryParams = z.object({
  id: zRouterId,
});

const Page = () => {
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

export default Page;
