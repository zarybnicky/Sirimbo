import { ScheduleForm } from "components/ScheduleForm";
import { useScheduleQuery } from "lib/graphql/Schedule";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ScheduleEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useScheduleQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <ScheduleForm data={data.rozpi || undefined} onSuccess={() => router.back()} />}
  </div>;
};
