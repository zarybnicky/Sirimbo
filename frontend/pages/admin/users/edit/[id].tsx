import { Container } from "@mui/material";
import { UserForm } from "components/UserForm";
import { useUserQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function UserEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useUserQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data && <UserForm data={data.user || undefined} onSuccess={() => router.back()} />}
  </Container>;
};
