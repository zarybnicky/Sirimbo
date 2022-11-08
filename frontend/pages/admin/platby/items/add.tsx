import { Container } from "@mui/material";
import { PaymentItemForm } from "components/PaymentItemForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentItemAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <PaymentItemForm onSuccess={() => router.back()} />
  </Container>;
};
