import { Container } from "@mui/material";
import { VideoForm } from "components/VideoForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function VideoAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <VideoForm onSuccess={() => router.back()} />
  </Container>;
};
