import { VideoSourceForm } from "components/VideoSourceForm";
import { useVideoSourceQuery } from "lib/graphql/Video";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function VideoSourceEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useVideoSourceQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <VideoSourceForm data={data?.videoSource || undefined} onSuccess={() => router.back()} />}
  </div>;
};
