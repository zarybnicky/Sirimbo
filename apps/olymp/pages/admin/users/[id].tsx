import { UserForm } from '@app/ui/UserForm';
import { useRouter } from 'next/router';
import { UserList } from '@app/ui/UserList';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { User } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peUsers, PermissionLevel.P_OWNED]}>
    <NextSeo title="Uživatelé" />
    <WithSidebar sidebar={<UserList />}>
      <UserForm entity={User} id={fromSlugArray(useRouter().query.id)} />
    </WithSidebar>
  </Layout>
);

export default Page;
