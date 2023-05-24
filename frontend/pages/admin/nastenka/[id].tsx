import { AnnouncementForm } from 'components/AnnouncementForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { fromSlugArray } from 'lib/slugify';
import { NextPageWithLayout } from 'pages/_app';
import { useGqlMutation, useGqlQuery } from 'lib/query';
import {
  AnnouncementDocument,
  DeleteAnnouncementDocument,
} from 'lib/graphql/Announcement';
import { AnnouncementList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useGqlQuery(
    AnnouncementDocument,
    { id },
    { enabled: !!id, cacheTime: 0 },
  );
  const { mutateAsync: doDelete } = useGqlMutation(DeleteAnnouncementDocument, {
    onSuccess: () => router.push('/admin/nastenka'),
  });
  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/nastenka"
        title={data?.upozorneni?.upNadpis || '(Bez názvu)'}
      >
        <DeleteButton onDelete={() => doDelete({ id })} title="smazat příspěvek" />
      </Item.Titlebar>
      {data && <AnnouncementForm data={data.upozorneni || undefined} />}
    </Item>
  );
};

Page.list = <AnnouncementList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_OWNED];
Page.staticTitle = 'Nástěnka';

export default Page;
