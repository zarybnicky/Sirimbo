import { GalleryDirForm } from "components/GalleryDirectoryForm";
import { useGalleryDirQuery } from "lib/graphql/Gallery";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function GalleryDirEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useGalleryDirQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <div className="container mx-auto max-w-3xl mt-12 mb-8">
    {data?.galerieDir && <GalleryDirForm data={data.galerieDir} onSuccess={() => router.back()} />}
  </div>;
};
