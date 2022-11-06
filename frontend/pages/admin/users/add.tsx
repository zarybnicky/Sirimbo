import { Container } from "@mui/material";
import { UserForm } from "components/UserForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function UserAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <UserForm onSuccess={() => router.back()} />
  </Container>;
};
