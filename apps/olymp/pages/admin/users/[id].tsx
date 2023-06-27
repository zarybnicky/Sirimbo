import { UserForm } from '@app/ui/UserForm';
import { useRouter } from 'next/router';
import { UserList } from '@app/ui/UserList';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <UserForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <UserList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];
Page.staticTitle = "Uživatelé";

export default Page;
