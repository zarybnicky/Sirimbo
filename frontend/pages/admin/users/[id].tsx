import { UserForm } from "components/UserForm";
import { DeleteButton } from "components/DeleteButton";
import { useUserQuery, useDeleteUserMutation } from "lib/graphql/User";
import { useRouter } from "next/router";
import { Layout } from 'components/layout/Layout';
import { Item } from 'components/layout/Item';
import { UserList } from 'components/UserList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function UserEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useUserQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteUserMutation({
    onSuccess: () => router.push('/admin/users'),
  });
  return <Item>
    <Item.Titlebar backHref="/admin/users" title={
      `${data?.user?.uJmeno} ${data?.user?.uPrijmeni}` || '(Bez názvu)'
    }>
      <DeleteButton onDelete={() => doDelete({ id: id as string })} title="smazat uživatele" />
    </Item.Titlebar>
    {data && <UserForm data={data.user || undefined} />}
  </Item>;
};

UserEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<UserList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peUsers, PermissionLevel.P_OWNED,
);
