import { Container } from "@mui/material";
import { PaymentCategoryForm } from "components/PaymentCategoryForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentCategoryAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <PaymentCategoryForm onSuccess={() => router.back()} />
  </Container>;
};
