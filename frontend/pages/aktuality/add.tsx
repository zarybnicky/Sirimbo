import { ArticleForm } from '@/ui/ArticleForm';
import { ArticleList } from '@/ui/ArticleList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';

const Page = () => (
  <Layout requireTrainer>
    <NextSeo title="Aktuality" />
    <WithSidebar sidebar={<ArticleList />}>
      <ArticleForm />;
    </WithSidebar>
  </Layout>
);

export default Page;
