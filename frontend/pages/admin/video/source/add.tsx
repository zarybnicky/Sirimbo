import { VideoSourceForm } from "components/VideoSourceForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function VideoSourceAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <VideoSourceForm onSuccess={() => router.back()} />
  </div>;
};
