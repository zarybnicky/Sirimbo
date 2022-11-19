import { ReservationForm } from "components/ReservationForm";
import { useReservationQuery } from "lib/graphql/Reservation";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function ReservationEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useReservationQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <ReservationForm data={data.nabidka || undefined} onSuccess={() => router.back()} />}
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNabidka, PermissionLevel.P_OWNED,
);
