import { CohortGroupForm } from '@/ui/CohortGroupForm';
import { CohortGroupList } from '@/ui/CohortGroupList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';

const Page = () => (
  <Layout requireAdmin>
    <NextSeo title="Tréninkové programy" />
    <WithSidebar sidebar={<CohortGroupList />}>
      <CohortGroupForm />
    </WithSidebar>
  </Layout>
);


export default Page;
