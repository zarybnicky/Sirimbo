import { UserList } from '@app/ui/UserList';
import { WithSidebar } from '@app/ui/WithSidebar';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => (
  <Layout permissions={[PermissionKey.peUsers, PermissionLevel.P_OWNED]}>
    <NextSeo title="Uživatelé" />
    <WithSidebar sidebar={<UserList />} />
  </Layout>
);

export default Page;
