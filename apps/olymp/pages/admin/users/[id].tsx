import { UserForm } from '@app/ui/UserForm';
import { useRouter } from 'next/router';
import { UserList } from '@app/ui/UserList';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { User } from '@app/ui/entities';

const Page = () => <UserForm entity={User} id={fromSlugArray(useRouter().query.id)} />;

Page.list = <UserList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];
Page.staticTitle = "Uživatelé";

export default Page;
