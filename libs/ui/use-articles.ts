import { ArticlesDocument } from '@app/graphql/Articles';
import { slugify } from '@app/ui/slugify';
import { Route } from 'nextjs-routes';
import { useQuery } from 'urql';

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
  const [{ data }] = useQuery({query: ArticlesDocument, variables: { first: limit, offset }});
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
