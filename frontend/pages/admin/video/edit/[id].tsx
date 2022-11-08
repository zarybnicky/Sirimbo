import { Container } from "@mui/material";
import { VideoForm } from "components/VideoForm";
import { useVideoQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function VideoEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useVideoQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <VideoForm data={data?.video || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
