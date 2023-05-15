import { UserForm } from 'components/UserForm';
import { DeleteButton } from 'components/DeleteButton';
import { useUserQuery, useDeleteUserMutation } from 'lib/graphql/User';
import { useRouter } from 'next/router';
import { Item } from 'components/layout/Item';
import { UserList } from 'components/UserList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useUserQuery({ id }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteUserMutation({
    onSuccess: () => router.push('/admin/users'),
  });
  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/users"
        title={`${data?.user?.uJmeno} ${data?.user?.uPrijmeni}` || '(Bez názvu)'}
      >
        <DeleteButton onDelete={() => doDelete({ id })} title="smazat uživatele" />
      </Item.Titlebar>
      {data && <UserForm data={data.user || undefined} />}
    </Item>
  );
}

Page.list = <UserList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];

export default Page;
