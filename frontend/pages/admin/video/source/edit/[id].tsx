import { Container } from "@mui/material";
import { VideoSourceForm } from "components/VideoSourceForm";
import { useVideoSourceQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function VideoSourceEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useVideoSourceQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <VideoSourceForm data={data?.videoSource || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
