import { ArticleForm } from 'components/ArticleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';
import { ArticleList } from 'lib/entity-lists';
import { ArticleDocument } from 'lib/graphql/Articles';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({query: ArticleDocument, variables: { id }, pause: !id});
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      {data && (
        <ArticleForm
          data={data?.aktuality || undefined}
          onSuccess={() => router.back()}
        />
      )}
    </div>
  );
}

Page.list = <ArticleList />;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Aktuality";
Page.isDetail = true;

export default Page;
