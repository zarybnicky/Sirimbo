import { PaymentCategoryForm } from "components/PaymentCategoryForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function PaymentCategoryAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <PaymentCategoryForm onSuccess={() => router.back()} />
  </div>;
};
