import { FileForm } from 'components/FileForm';
import { useFileQuery } from 'lib/graphql/Documents';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';

export default function FileEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useFileQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return (
    <div className="container mx-auto max-w-3xl mt-12 mb-8">
      {data?.dokumenty && (
        <FileForm data={data.dokumenty} onSuccess={() => router.back()} />
      )}
    </div>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peDokumenty,
  PermissionLevel.P_OWNED,
);
