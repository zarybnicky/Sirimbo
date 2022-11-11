import { PaymentItemForm } from "components/PaymentItemForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentItemAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <PaymentItemForm onSuccess={() => router.back()} />
  </div>;
};
