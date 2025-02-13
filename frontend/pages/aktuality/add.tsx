import { ArticleForm } from '@/ui/forms/ArticleForm';
import { ArticleList } from '@/ui/lists/ArticleList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';

function Page() {
  return <Layout requireTrainer>
    <NextSeo title="Aktuality" />
    <WithSidebar sidebar={<ArticleList />}>
      <ArticleForm />;
    </WithSidebar>
  </Layout>
}

export default Page;
