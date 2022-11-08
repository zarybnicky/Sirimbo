import { Container } from "@mui/material";
import { ReservationForm } from "components/ReservationForm";
import { useReservationQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ReservationEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useReservationQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <ReservationForm data={data.nabidka || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
