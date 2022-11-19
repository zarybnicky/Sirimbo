import { GalleryPhotoForm } from "components/GalleryPhotoForm";
import { useGalleryPhotoQuery } from "lib/graphql/Gallery";
import { useRouter } from "next/router";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function GalleryPhotoEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useGalleryPhotoQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data?.galerieFoto && <GalleryPhotoForm data={data.galerieFoto} onSuccess={() => router.back()} />}
  </div>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peGalerie, PermissionLevel.P_OWNED,
);
