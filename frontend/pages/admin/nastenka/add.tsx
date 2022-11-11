import { AnnouncementForm } from "components/AnnouncementForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function AnnouncementAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <AnnouncementForm onSuccess={() => router.back()} />
  </div>;
};
