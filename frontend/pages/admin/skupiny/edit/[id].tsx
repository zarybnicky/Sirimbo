import { Container } from "@mui/material";
import { CohortForm } from "components/CohortForm";
import { useCohortQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function CohortEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useCohortQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <CohortForm data={data.skupiny || undefined} onSuccess={() => router.back()} />}
  </Container>;
};