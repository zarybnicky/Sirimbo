import { UserForm } from 'components/UserForm';
import { DeleteButton } from 'components/DeleteButton';
import { DeleteUserDocument, UserDocument } from 'lib/graphql/User';
import { useRouter } from 'next/router';
import { Item } from 'components/layout/Item';
import { UserList } from 'components/UserList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({query: UserDocument, variables: { id }, pause: !!id });
  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/users"
        title={`${data?.user?.uJmeno} ${data?.user?.uPrijmeni}` || '(Bez názvu)'}
      >
        <DeleteButton
          doc={DeleteUserDocument}
          id={id}
          onDelete={() => router.push('/admin/users')}
          title="smazat uživatele"
        />
      </Item.Titlebar>
      {data && <UserForm data={data.user || undefined} />}
    </Item>
  );
}

Page.list = <UserList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peUsers, PermissionLevel.P_OWNED];
Page.staticTitle = "Uživatelé";

export default Page;
