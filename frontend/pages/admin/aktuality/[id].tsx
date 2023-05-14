import { ArticleForm } from 'components/ArticleForm';
import { useArticleQuery } from 'lib/graphql/Articles';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { type NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';
import { ArticleList } from 'components/ArticleList';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useArticleQuery({ id }, { enabled: !!id, cacheTime: 0 });
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

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAktuality,
  PermissionLevel.P_OWNED,
);
