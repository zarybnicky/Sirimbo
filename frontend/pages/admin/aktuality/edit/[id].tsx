import { ArticleForm } from "components/ArticleForm";
import { useArticleQuery } from "lib/graphql/Articles";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function ArticleEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useArticleQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <ArticleForm data={data?.aktuality || undefined} onSuccess={() => router.back()} />}
  </div>;
};
