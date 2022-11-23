import { UserForm } from "components/UserForm";
import { Layout } from 'components/layout/Layout';
import { UserList } from 'components/UserList';
import { withServerPermissions, PermissionKey, PermissionLevel } from "lib/data/use-server-permissions";
import { Item } from "components/layout/Item";

export default function UserAddPage() {
  return <Item>
    <Item.Titlebar backHref="/admin/users" title="Nový uživatel" />
    <UserForm />
  </Item>;
};

UserAddPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<UserList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peUsers, PermissionLevel.P_OWNED,
);
