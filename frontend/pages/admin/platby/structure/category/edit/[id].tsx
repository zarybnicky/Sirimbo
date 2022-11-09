import { Container } from "@mui/material";
import { PaymentCategoryForm } from "components/PaymentCategoryForm";
import { usePaymentCategoryQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentCategoryEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = usePaymentCategoryQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <PaymentCategoryForm data={data.platbyCategory || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
