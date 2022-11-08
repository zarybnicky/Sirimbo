import { Container } from "@mui/material";
import { ScheduleForm } from "components/ScheduleForm";
import { useScheduleQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ScheduleEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useScheduleQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <ScheduleForm data={data.rozpi || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
