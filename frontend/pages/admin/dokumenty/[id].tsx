import { FileForm } from 'components/FileForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';
import { useGqlQuery } from 'lib/query';
import { FileDocument } from 'lib/graphql/Documents';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
    const { data } = useGqlQuery(FileDocument, { id }, { enabled: !!id, cacheTime: 0 });
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      {data?.dokumenty && (
        <FileForm data={data.dokumenty} onSuccess={() => router.back()} />
      )}
    </div>
  );
}

Page.permissions = [PermissionKey.peDokumenty, PermissionLevel.P_OWNED];
Page.staticTitle = "Dokumenty";

export default Page;
