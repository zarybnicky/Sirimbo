import { Container } from "@mui/material";
import { GalleryDirForm } from "components/GalleryDirectoryForm";
import { useGalleryDirQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";
import { useRouter } from "next/router";

export default function GalleryDirEditPage() {
  useRequireUserLoggedIn();
  const router = useRouter();
  const { id } = router.query;
  const { data } = useGalleryDirQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  return <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
    {data?.galerieDir && <GalleryDirForm data={data.galerieDir} onSuccess={() => router.back()} />}
  </Container>;
};
