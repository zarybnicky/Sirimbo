import { UserList } from '@app/ui/UserList';
import { WithSidebar } from '@app/ui/WithSidebar';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => (
  <Layout>
    <NextSeo title="Uživatelé" />
    <WithSidebar sidebar={<UserList />} />
  </Layout>
);

export default Page;
