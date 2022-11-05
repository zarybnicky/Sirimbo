import { Container } from "@mui/material";
import { PaymentGroupForm } from "components/PaymentGroupForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentGroupAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <PaymentGroupForm onSuccess={() => router.back()} />
  </Container>;
};
