import { Container } from "@mui/material";
import { PaymentGroupForm } from "components/PaymentGroupForm";
import { usePaymentGroupQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentGroupEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = usePaymentGroupQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <PaymentGroupForm data={data.platbyGroup || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
