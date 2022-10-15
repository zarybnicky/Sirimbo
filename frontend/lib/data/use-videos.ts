import { useTitleVideosQuery } from 'lib/graphql';

export interface Video {
  img: string;
  href: string;
  name: string;
}

export const useTitleVideos = (): Video[] => {
  const { data } = useTitleVideosQuery();
  return (data?.titleVideos?.nodes || []).map(x => {
    const [id, query] = x.vUri.split('?');
    return {
      name: x.vTitle,
      href: `https://www.youtube.com/watch?v=${id}` + (query ? `&amp;${query}` : ''),
      img: `https://i3.ytimg.com/vi/${id}/hqdefault.jpg`,
    };
  });
};
