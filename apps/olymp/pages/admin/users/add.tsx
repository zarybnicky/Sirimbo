import { UserForm } from '@app/ui/UserForm';
import { UserList } from '@app/ui/UserList';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { User } from 'lib/entities';

const Page: NextPageWithLayout = () => <UserForm entity={User} />;

Page.list = <UserList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];
Page.staticTitle = "Uživatelé";

export default Page;
