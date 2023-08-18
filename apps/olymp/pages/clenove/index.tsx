import { PersonList } from '@app/ui/PersonList';
import { WithSidebar } from '@app/ui/WithSidebar';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => (
  <Layout>
    <NextSeo title="Členové" />
    <WithSidebar sidebar={<PersonList />} />
  </Layout>
);

export default Page;
