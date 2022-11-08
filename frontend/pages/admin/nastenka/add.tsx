import { Container } from "@mui/material";
import { AnnouncementForm } from "components/AnnouncementForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function AnnouncementAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <AnnouncementForm onSuccess={() => router.back()} />
  </Container>;
};
