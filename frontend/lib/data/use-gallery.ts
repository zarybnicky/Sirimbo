import format from 'date-fns/format';
import { useGalleryDirQuery } from 'lib/graphql';

export interface GalleryDir {
  name?: string;
  parentId?: string;
}

export interface GalleryItem {
  id: string;
  name: string;
  href: string;
  date: string;
  img: string;
  imgThumb: string;
}

export const useGallery = (dir: string): {
  dir?: GalleryDir;
  dirs: GalleryItem[];
  images: GalleryItem[];
} => {
  const { data } = useGalleryDirQuery({ id: dir });

  const dirs = (data?.galerieDirs?.nodes || []).map((x) => {
    const date = x.galerieFotosByGfIdRodic.nodes?.[0]?.gfTimestamp;
    const photo = x.galerieFotosByGfIdRodic.nodes?.[0]?.gfPath;
    return {
      id: x.gdId,
      name: x.gdName,
      href: `/gallery/${x.gdId}`,
      date: date ? format(new Date(date), 'd. M. y') : '',
      img: decodeURIComponent(`/galerie/${photo}`),
      imgThumb: decodeURIComponent(`/galerie/thumbnails/${photo}`),
    };
  }).filter(x => x.id != dir);

  const images = (data?.galerieDir?.galerieFotosByGfIdRodic.nodes || []).map(x => {
    return {
      id: x.gfId,
      name: x.gfName,
      href: `/gallery/${x.gfIdRodic}/photo/${x.gfId}`,
      date: x.gfTimestamp ? format(new Date(x.gfTimestamp), 'd. M. y') : '',
      img: decodeURIComponent(`/galerie/${x.gfPath}`),
      imgThumb: decodeURIComponent(`/galerie/thumbnails/${x.gfPath}`),
    };
  });

  return {
    dir: {
      name: data?.galerieDir?.gdName,
      parentId: (
        data?.galerieDir?.gdIdRodic && data?.galerieDir?.gdIdRodic != dir
          ? data?.galerieDir?.gdIdRodic
          : undefined
      ),
    },
    dirs,
    images,
  };
};
