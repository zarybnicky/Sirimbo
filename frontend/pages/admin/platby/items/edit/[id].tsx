import { Container } from "@mui/material";
import { PaymentItemForm } from "components/PaymentItemForm";
import { usePaymentItemQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentItemEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = usePaymentItemQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <PaymentItemForm data={data.platbyItem || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
