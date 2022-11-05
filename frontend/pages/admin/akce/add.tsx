import { Container } from "@mui/material";
import { EventForm } from "components/EventForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function EventAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <EventForm onSuccess={() => router.back()} />
  </Container>;
};
