import { ReservationForm } from "components/ReservationForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ReservationAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <ReservationForm onSuccess={() => router.back()} />
  </div>;
};
