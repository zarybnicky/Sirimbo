import { CohortGroupForm } from '@app/ui/CohortGroupForm';
import { CohortGroupList } from '@app/ui/CohortGroupList';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireAdmin>
    <NextSeo title="Tréninkové programy" />
    <WithSidebar sidebar={<CohortGroupList />}>
      <CohortGroupForm />
    </WithSidebar>
  </Layout>
);


export default Page;
