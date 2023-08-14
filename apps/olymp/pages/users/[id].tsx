import { UserForm } from '@app/ui/UserForm';
import { useRouter } from 'next/router';
import { UserList } from '@app/ui/UserList';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireTrainer>
    <NextSeo title="Uživatelé" />
    <WithSidebar sidebar={<UserList />}>
      <UserForm id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);

export default Page;
