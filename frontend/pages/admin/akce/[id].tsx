import { EventForm } from 'components/EventForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import type { NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';
import { useGqlMutation, useGqlQuery } from 'lib/query';
import { DeleteEventDocument, EventDocument } from 'lib/graphql/Event';
import { EventList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useGqlQuery(EventDocument, { id }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useGqlMutation(DeleteEventDocument, {
    onSuccess: () => router.push('/admin/akce'),
  });
  return (
    <Item>
      <Item.Titlebar backHref="/admin/users" title={data?.event?.name || '(Bez názvu)'}>
        <DeleteButton onDelete={() => doDelete({ id })} title="smazat uživatele" />
      </Item.Titlebar>
      {data && <EventForm data={data?.event || undefined} />}
    </Item>
  );
};

Page.list = <EventList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAkce, PermissionLevel.P_OWNED];
Page.staticTitle = 'Akce';

export default Page;
