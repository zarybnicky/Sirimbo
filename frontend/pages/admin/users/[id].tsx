import { UserForm } from "components/UserForm";
import { useUserQuery } from "lib/graphql/User";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function UserEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useUserQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <UserForm data={data.user || undefined} onSuccess={() => router.back()} />}
  </div>;
};
