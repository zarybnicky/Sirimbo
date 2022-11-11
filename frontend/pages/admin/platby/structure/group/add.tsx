import { PaymentGroupForm } from "components/PaymentGroupForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentGroupAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <PaymentGroupForm onSuccess={() => router.back()} />
  </div>;
};
