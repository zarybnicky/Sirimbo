import { getPlaceholder } from '../test-utils';
import { useTypedQuery } from '../zeus/apollo';

export interface Video {
  img: string;
  href: string;
  name: string;
}

export const useTitleVideos = (): Video[] => {
  const { data } = useTypedQuery({
    titleVideos: [{}, {
      nodes: {
        vTitle: true,
        vUri: true,
      },
    }],
  });

  return (data?.titleVideos?.nodes || []).map(x => {
    const [id, query] = x.vUri.split('?');
    return {
      name: x.vTitle,
      href: `https://www.youtube.com/watch?v=${id}` + (query ? `&amp;${query}` : ''),
      img: `https://i3.ytimg.com/vi/${id}/hqdefault.jpg`,
    };
  });
};

export const useMockVideos = (): Video[] => [
  {
    href: "/o-nas",
    img: getPlaceholder(360, 240),
    name: "Přípravný kurz tanečního sportu",
  },
  {
    href: "/o-nas",
    img: getPlaceholder(360, 240),
    name: "Přípravný kurz tanečního sportu",
  },
  {
    href: "/o-nas",
    img: getPlaceholder(360, 240),
    name: "Přípravný kurz tanečního sportu",
  },
  {
    href: "/o-nas",
    img: getPlaceholder(360, 240),
    name: "Přípravný kurz tanečního sportu",
  },
];
