import { ReservationForm } from 'components/ReservationForm';
import {
  useDeleteReservationMutation,
  useReservationQuery,
} from 'lib/graphql/Reservation';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { ReservationList } from 'components/ReservationList';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useReservationQuery({ id }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteReservationMutation({
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

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNabidka,
  PermissionLevel.P_OWNED,
);