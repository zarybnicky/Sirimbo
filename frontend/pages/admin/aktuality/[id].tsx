import { ArticleForm } from 'components/ArticleForm';
import { useArticleQuery } from 'lib/graphql/Articles';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';

export default function ArticleEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useArticleQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
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

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAktuality,
  PermissionLevel.P_OWNED,
);
