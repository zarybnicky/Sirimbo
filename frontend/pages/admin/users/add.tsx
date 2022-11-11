import { UserForm } from "components/UserForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function UserAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <UserForm onSuccess={() => router.back()} />
  </div>;
};
