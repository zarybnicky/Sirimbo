import { Container } from "@mui/material";
import { GalleryPhotoForm } from "components/GalleryPhotoForm";
import { useGalleryPhotoQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function GalleryPhotoEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useGalleryPhotoQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data?.galerieFoto && <GalleryPhotoForm data={data.galerieFoto} onSuccess={() => router.back()} />}
  </Container>;
};
