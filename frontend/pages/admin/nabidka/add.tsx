import { Container } from "@mui/material";
import { ReservationForm } from "components/ReservationForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ReservationAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <ReservationForm onSuccess={() => router.back()} />
  </Container>;
};
