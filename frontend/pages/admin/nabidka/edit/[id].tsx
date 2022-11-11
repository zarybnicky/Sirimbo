import { ReservationForm } from "components/ReservationForm";
import { useReservationQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ReservationEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useReservationQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <ReservationForm data={data.nabidka || undefined} onSuccess={() => router.back()} />}
  </div>;
};
