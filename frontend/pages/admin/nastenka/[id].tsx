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
import { AnnouncementList } from 'components/AnnouncementList';
import { fromSlugArray } from 'lib/slugify';
import { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useAnnouncementQuery({ id }, { enabled: !!id, cacheTime: 0 },);
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
          onDelete={() => doDelete({ id })}
          title="smazat příspěvek"
        />
      </Item.Titlebar>
      {data && <AnnouncementForm data={data.upozorneni || undefined} />}
    </Item>
  );
}

Page.list = <AnnouncementList />;
Page.isDetail = true;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka,
  PermissionLevel.P_OWNED,
);
