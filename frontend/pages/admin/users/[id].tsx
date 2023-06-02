import { UserForm } from 'components/UserForm';
import { useRouter } from 'next/router';
import { UserList } from 'components/UserList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <UserForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <UserList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];
Page.staticTitle = "Uživatelé";

export default Page;
