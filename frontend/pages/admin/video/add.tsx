import { VideoForm } from "components/VideoForm";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function VideoAddPage() {
  const router = useRouter();
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    <VideoForm onSuccess={() => router.back()} />
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peGalerie, PermissionLevel.P_OWNED,
);
