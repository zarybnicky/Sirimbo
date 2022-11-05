import { Container } from "@mui/material";
import { CohortForm } from "components/CohortForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function CohortAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <CohortForm onSuccess={() => router.back()} />
  </Container>;
};
