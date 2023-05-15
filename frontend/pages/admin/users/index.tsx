import { UserList } from 'components/UserList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <UserList />;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];
Page.staticTitle = "Uživatelé";

export default Page;
