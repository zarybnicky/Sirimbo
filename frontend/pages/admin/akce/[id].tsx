import { EventForm } from 'components/EventForm';
import { useDeleteEventMutation, useEventQuery } from 'lib/graphql/Event';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { EventList } from 'components/EventList';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { type NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useEventQuery({ id }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteEventMutation({
    onSuccess: () => router.push('/admin/akce'),
  });
  return (
    <Item>
      <Item.Titlebar backHref="/admin/users" title={data?.event?.name || '(Bez názvu)'}>
        <DeleteButton
          onDelete={() => doDelete({ id })}
          title="smazat uživatele"
        />
      </Item.Titlebar>
      {data && <EventForm data={data?.event || undefined} />}
    </Item>
  );
}

Page.list = <EventList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce,
  PermissionLevel.P_OWNED,
);
