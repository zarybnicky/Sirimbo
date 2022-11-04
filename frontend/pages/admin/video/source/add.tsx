import { Container } from "@mui/material";
import { VideoSourceForm } from "components/VideoSourceForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function VideoSourceAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <VideoSourceForm onSuccess={() => router.back()} />
  </Container>;
};
