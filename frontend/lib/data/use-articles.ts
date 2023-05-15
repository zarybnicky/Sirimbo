import { ArticlesDocument } from 'lib/graphql/Articles';
import { useGqlQuery } from 'lib/query';
import { slugify } from 'lib/slugify';
import { Route } from 'nextjs-routes';

export interface Article {
  id: string;
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
  const { data } = useGqlQuery(ArticlesDocument, { limit, offset });
  return {
    articles: (data?.aktualities?.nodes || []).map((x) => ({
      id: x.id,
      href: {
        pathname: '/articles/[id]/[...slug]',
        query: { id: x.id, slug: [slugify(x.atJmeno)] },
      },
      img: `/galerie/${x.galerieFotoByAtFotoMain?.gfPath}`,
      imgThumb: `/galerie/thumbnails/${x.galerieFotoByAtFotoMain?.gfPath}`,
      header: x.atJmeno,
      preview: x.atPreview,
    })),
    count: data?.aktualities?.totalCount || 0,
  };
};
