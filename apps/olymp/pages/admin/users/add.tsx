import { UserForm } from '@app/ui/UserForm';
import { UserList } from '@app/ui/UserList';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { User } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';


const Page = () => (
  <Layout permissions={[PermissionKey.peUsers, PermissionLevel.P_OWNED]}>
    <NextSeo title="Uživatelé" />
    <WithSidebar sidebar={<UserList />}>
      <UserForm entity={User} />
    </WithSidebar>
  </Layout>
);

export default Page;
