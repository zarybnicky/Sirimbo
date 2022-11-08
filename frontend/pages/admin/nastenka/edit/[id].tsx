import { Container } from "@mui/material";
import { AnnouncementForm } from "components/AnnouncementForm";
import { useAnnouncementQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function AnnouncementEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useAnnouncementQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <AnnouncementForm data={data.upozorneni || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
