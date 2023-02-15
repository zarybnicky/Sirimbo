import { fullDateFormatter } from 'lib/format-date';
import { useGalleryDirQuery } from 'lib/graphql/Gallery';

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
      id: x.id,
      name: x.gdName,
      href: `/gallery/${x.id}`,
      date: date ? fullDateFormatter.format(new Date(date)) : '',
      img: decodeURIComponent(`/galerie/${photo}`),
      imgThumb: decodeURIComponent(`/galerie/thumbnails/${photo}`),
    };
  }).filter(x => x.id != dir);

  const images = (data?.galerieDir?.galerieFotosByGfIdRodic.nodes || []).map(x => {
    return {
      id: x.id,
      name: x.gfName,
      href: `/gallery/${x.gfIdRodic}/photo/${x.id}`,
      date: x.gfTimestamp ? fullDateFormatter.format(new Date(x.gfTimestamp)) : '',
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
