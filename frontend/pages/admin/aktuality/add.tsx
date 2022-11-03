import { Container } from "@mui/material";
import { ArticleForm } from "components/ArticleForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ArticleAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <ArticleForm onSuccess={() => router.back()} />
  </Container>;
};
