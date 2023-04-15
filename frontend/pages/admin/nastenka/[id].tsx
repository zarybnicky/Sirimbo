import { AnnouncementForm } from 'components/AnnouncementForm';
import {
  useAnnouncementQuery,
  useDeleteAnnouncementMutation,
} from 'lib/graphql/Announcement';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { Layout } from 'components/layout/Layout';
import { AnnouncementList } from 'components/AnnouncementList';

export default function AnnouncementEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useAnnouncementQuery(
    { id: id as string },
    { enabled: !!id, cacheTime: 0 },
  );
  const { mutateAsync: doDelete } = useDeleteAnnouncementMutation({
    onSuccess: () => router.push('/admin/nastenka'),
  });
  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/nastenka"
        title={data?.upozorneni?.upNadpis || '(Bez názvu)'}
      >
        <DeleteButton
          onDelete={() => doDelete({ id: id as string })}
          title="smazat příspěvek"
        />
      </Item.Titlebar>
      {data && <AnnouncementForm data={data.upozorneni || undefined} />}
    </Item>
  );
}

AnnouncementEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<AnnouncementList />} isDetail>
    {page}
  </Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka,
  PermissionLevel.P_OWNED,
);
