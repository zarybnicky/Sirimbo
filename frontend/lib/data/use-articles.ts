import { useArticlesQuery } from 'lib/graphql/Articles';
import { Route } from 'nextjs-routes';

export interface Article {
  href: Route;
  img: string;
  imgThumb: string;
  header: string;
  preview: string;
}

export const useArticles = (
  limit: number,
  offset: number,
): {
  articles: Article[];
  count: number;
} => {
  const { data } = useArticlesQuery({ limit, offset });
  return {
    articles: (data?.aktualities?.nodes || []).map((x) => ({
      href: { pathname: '/articles/[id]', query: { id: x.id } },
      img: `/galerie/${x.galerieFotoByAtFotoMain?.gfPath}`,
      imgThumb: `/galerie/thumbnails/${x.galerieFotoByAtFotoMain?.gfPath}`,
      header: x.atJmeno,
      preview: x.atPreview,
    })),
    count: data?.aktualities?.totalCount || 0,
  };
};
