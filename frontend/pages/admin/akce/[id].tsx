import { EventForm } from 'components/EventForm';
import { useDeleteEventMutation, useEventQuery } from 'lib/graphql/Event';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { EventList } from 'components/EventList';
import { Layout } from 'components/layout/Layout';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';

export default function EventEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useEventQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteEventMutation({
    onSuccess: () => router.push('/admin/akce'),
  });
  return (
    <Item>
      <Item.Titlebar backHref="/admin/users" title={data?.akce?.aJmeno || '(Bez názvu)'}>
        <DeleteButton
          onDelete={() => doDelete({ id: id as string })}
          title="smazat uživatele"
        />
      </Item.Titlebar>
      {data && <EventForm data={data?.akce || undefined} />}
    </Item>
  );
}

EventEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<EventList />} isDetail>
    {page}
  </Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce,
  PermissionLevel.P_OWNED,
);
