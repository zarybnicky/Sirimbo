import { Container } from "@mui/material";
import { RoleForm } from "components/RoleForm";
import { useRoleQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function RoleEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useRoleQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <RoleForm data={data.permission || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
