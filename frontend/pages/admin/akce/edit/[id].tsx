import { EventForm } from "components/EventForm";
import { useEventQuery } from "lib/graphql/Event";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function EventEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useEventQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <EventForm data={data?.akce || undefined} onSuccess={() => router.back()} />}
  </div>;
};
