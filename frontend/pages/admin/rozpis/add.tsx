import { ScheduleForm } from "components/ScheduleForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ScheduleAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <ScheduleForm onSuccess={() => router.back()} />
  </div>;
};
