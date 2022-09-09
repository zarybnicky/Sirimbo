import { getPlaceholder } from '../test-utils';
import { $, GalerieFotosOrderBy, Selector } from 'lib/zeus';
import { useTypedQuery } from 'lib/zeus/apollo';
import format from 'date-fns/format';

const GalleryPhotoPartial = Selector('GalerieFoto')({
  __typename: true,
  nodeId: true,
  gfId: true,
  gfIdRodic: true,
  gfKdo: true,
  gfName: true,
  gfPath: true,
  gfTimestamp: true,
});

const GalleryDirPartial = Selector('GalerieDir')({
  __typename: true,
  nodeId: true,
  gdId: true,
  gdIdRodic: true,
  gdName: true,
  gdPath: true,
  gdLevel: true,
  gdHidden: true,
});

export const GalleryDirQuery = Selector('Query')({
  galerieDir: [
    { gdId: $`dirId` },
    {
      ...GalleryDirPartial,
      galerieFotosByGfIdRodic: [
        { orderBy: [GalerieFotosOrderBy.GF_NAME_ASC] },
        { nodes: GalleryPhotoPartial },
      ],
    },
  ],
  galerieDirs: [
    { condition: { gdIdRodic: $`dirId2`, gdHidden: false } },
    {
      nodes: {
        ...GalleryDirPartial,
        galerieFotosByGfIdRodic: [
          { orderBy: [GalerieFotosOrderBy.GF_NAME_ASC], first: 1 },
          { nodes: GalleryPhotoPartial },
        ],
      },
    },
  ],
});

export interface GalleryDir {
  name?: string;
  parentId?: string;
}

export interface GalleryItem {
  id: number;
  name: string;
  href: string;
  date: string;
  img: string;
  imgThumb: string;
}

export const useGallery = (dir: number): {
  dir?: GalleryDir;
  dirs: GalleryItem[];
  images: GalleryItem[];
} => {
  const { data } = useTypedQuery(GalleryDirQuery, {
    variables: {
      dirId: dir,
      dirId2: dir,
    },
  });
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
      date: format(new Date(x.gfTimestamp), 'd. M. y'),
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

export const useMockGallery = () => [
  {
    img: getPlaceholder(1600, 900),
    imgThumb: getPlaceholder(300, 200),
    href: "/o-nas",
    name: "Galerie Olomouc",
    date: "20. 10. 2019",
  },
  {
    img: getPlaceholder(1600, 900),
    imgThumb: getPlaceholder(300, 200),
    href: "/o-nas",
    name: "Galerie Olomouc",
    date: "20. 10. 2019",
  },
  {
    img: getPlaceholder(1600, 900),
    imgThumb: getPlaceholder(300, 200),
    href: "/o-nas",
    name: "Galerie Olomouc",
    date: "20. 10. 2019",
  },
  {
    img: getPlaceholder(1600, 900),
    imgThumb: getPlaceholder(300, 200),
    href: "/o-nas",
    name: "Galerie Olomouc",
    date: "20. 10. 2019",
  },
];
