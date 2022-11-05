import { Container } from "@mui/material";
import { RoleForm } from "components/RoleForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function RoleAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    <RoleForm onSuccess={() => router.back()} />
  </Container>;
};
