import { ReservationForm } from "components/ReservationForm";
import { useDeleteReservationMutation, useReservationQuery } from "lib/graphql/Reservation";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { Layout } from "components/layout/Layout";
import { ReservationList } from "components/ReservationList";
import { Item } from "components/layout/Item";
import { DeleteButton } from "components/DeleteButton";

export default function ReservationEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useReservationQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteReservationMutation({
    onSuccess: () => router.push('/admin/nabidka'),
  });
  return <Item>
    <Item.Titlebar backHref="/admin/nabidka" title={data?.nabidka?.userByNTrener?.fullName || '(Bez názvu)'}>
      <DeleteButton onDelete={() => doDelete({ id: id as string })} title="smazat příspěvek" />
    </Item.Titlebar>
    {data && <ReservationForm data={data.nabidka || undefined} />}
  </Item>;
};

ReservationEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<ReservationList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNabidka, PermissionLevel.P_OWNED,
);
