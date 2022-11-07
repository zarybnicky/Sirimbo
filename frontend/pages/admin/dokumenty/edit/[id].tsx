import { Container } from "@mui/material";
import { FileForm } from "components/FileForm";
import { useFileQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function FileEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useFileQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data?.dokumenty && <FileForm data={data.dokumenty} onSuccess={() => router.back()} />}
  </Container>;
};
