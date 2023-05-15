import { UserForm } from 'components/UserForm';
import { UserList } from 'components/UserList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/users" title="Nový uživatel" />
      <UserForm />
    </Item>
  );
}

Page.list = <UserList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];
Page.staticTitle = "Uživatelé";

export default Page;
