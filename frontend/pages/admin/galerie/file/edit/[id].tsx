import { GalleryPhotoForm } from "components/GalleryPhotoForm";
import { useGalleryPhotoQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function GalleryPhotoEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useGalleryPhotoQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data?.galerieFoto && <GalleryPhotoForm data={data.galerieFoto} onSuccess={() => router.back()} />}
  </div>;
};
