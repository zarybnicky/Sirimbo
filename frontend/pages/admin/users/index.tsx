import { Layout } from 'components/layout/Layout';
import { UserList } from 'components/UserList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <UserList />;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peUsers,
  PermissionLevel.P_OWNED,
);
