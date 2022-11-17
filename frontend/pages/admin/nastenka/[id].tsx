import { AnnouncementForm } from "components/AnnouncementForm";
import { useAnnouncementQuery } from "lib/graphql/Announcement";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function AnnouncementEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useAnnouncementQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <AnnouncementForm data={data.upozorneni || undefined} onSuccess={() => router.back()} />}
  </div>;
};
