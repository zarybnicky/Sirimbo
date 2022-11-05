import { Container } from "@mui/material";
import { EventForm } from "components/EventForm";
import { useEventQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function EventEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useEventQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <EventForm data={data?.akce || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
