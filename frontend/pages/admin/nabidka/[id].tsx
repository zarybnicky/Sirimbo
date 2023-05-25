import { ReservationForm } from 'components/ReservationForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { useGqlMutation, useGqlQuery } from 'lib/query';
import { DeleteReservationDocument, ReservationDocument } from 'lib/graphql/Reservation';
import { ReservationList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useGqlQuery(
    ReservationDocument,
    { id },
    { enabled: !!id, cacheTime: 0 },
  );
  const { mutateAsync: doDelete } = useGqlMutation(DeleteReservationDocument, {
    onSuccess: () => router.push('/admin/nabidka'),
  });
  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/nabidka"
        title={data?.nabidka?.userByNTrener?.fullName || '(Bez názvu)'}
      >
        <DeleteButton onDelete={() => doDelete({ id })} title="smazat příspěvek" />
      </Item.Titlebar>
      {data && <ReservationForm data={data.nabidka || undefined} />}
    </Item>
  );
};

Page.list = <ReservationList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNabidka, PermissionLevel.P_OWNED];
Page.staticTitle = 'Nabídky';

export default Page;
