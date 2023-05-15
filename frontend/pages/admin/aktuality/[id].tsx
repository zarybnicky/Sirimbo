import { ArticleForm } from 'components/ArticleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';
import { ArticleList } from 'components/ArticleList';
import { useGqlQuery } from 'lib/query';
import { ArticleDocument } from 'lib/graphql/Articles';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
    const { data } = useGqlQuery(ArticleDocument, { id }, { enabled: !!id, cacheTime: 0 });
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

export default Page;
