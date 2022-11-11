import { VideoForm } from "components/VideoForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function VideoAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <VideoForm onSuccess={() => router.back()} />
  </div>;
};
