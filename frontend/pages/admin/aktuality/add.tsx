import { ArticleForm } from "components/ArticleForm";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ArticleAddPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <ArticleForm onSuccess={() => router.back()} />
  </div>;
};
