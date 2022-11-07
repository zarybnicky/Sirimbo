import { Container } from "@mui/material";
import { DocumentForm } from "components/DocumentForm";
import { useDocumentQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function DocumentEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useDocumentQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data?.dokumenty && <DocumentForm data={data.dokumenty} onSuccess={() => router.back()} />}
  </Container>;
};
