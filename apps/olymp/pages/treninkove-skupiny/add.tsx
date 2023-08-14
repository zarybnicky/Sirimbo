import { CohortForm } from '@app/ui/CohortForm';
import { CohortList } from '@app/ui/CohortList';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireTrainer>
    <NextSeo title="Skupiny" />
    <WithSidebar sidebar={<CohortList />}>
      <CohortForm />
    </WithSidebar>
  </Layout>
);

export default Page;
