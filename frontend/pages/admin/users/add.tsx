import { UserForm } from 'components/UserForm';
import { UserList } from 'components/UserList';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
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

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peUsers,
  PermissionLevel.P_OWNED,
);
