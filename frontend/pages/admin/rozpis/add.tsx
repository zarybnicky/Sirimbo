import { Container } from "@mui/material";
import { ScheduleForm } from "components/ScheduleForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ScheduleAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <ScheduleForm onSuccess={() => router.back()} />
  </Container>;
};
