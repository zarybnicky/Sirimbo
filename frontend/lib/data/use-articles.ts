import { useArticlesQuery } from 'index';

export interface Article {
  href: string;
  img: string;
  imgThumb: string;
  header: string;
  preview: string;
}

export const useArticles = (limit: number, offset: number): {
  articles: Article[];
  count: number;
} => {
  const { data } = useArticlesQuery({ limit, offset });
  return {
    articles: (data?.aktualities?.nodes || []).map(x => ({
      href: `/aktualne/${x.atId}`,
      img: `/galerie/${x.galerieFotoByAtFotoMain?.gfPath}`,
      imgThumb: `/galerie/thumbnails/${x.galerieFotoByAtFotoMain?.gfPath}`,
      header: x.atJmeno,
      preview: x.atPreview,
    })),
    count: data?.aktualities?.totalCount || 0,
  };
};
