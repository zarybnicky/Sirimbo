import { Layout } from 'components/layout/Layout';
import { UserList } from 'components/UserList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function UserPage() {
  return null;
}

UserPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<UserList />}>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peUsers, PermissionLevel.P_OWNED,
);
