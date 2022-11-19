import { VideoForm } from "components/VideoForm";
import { useVideoQuery } from "lib/graphql/Video";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function VideoEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useVideoQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data && <VideoForm data={data?.video || undefined} onSuccess={() => router.back()} />}
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peGalerie, PermissionLevel.P_OWNED,
);
