import { $, Selector, AktualitiesOrderBy } from 'lib/zeus';
import { useTypedQuery } from 'lib/query';

export const ArticlesQuery = Selector('Query')({
  aktualities: [
    {
      first: $('limit', 'Int!'),
      offset: $('offset', 'Int!'),
      orderBy: [AktualitiesOrderBy.AT_TIMESTAMP_ADD_DESC],
    },
    {
      nodes: {
        atId: true,
        atPreview: true,
        atTimestampAdd: true,
        atJmeno: true,
        galerieFotoByAtFotoMain: {
          gfPath: true,
        }
      },
      totalCount: true,
    },
  ],
});

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
  const { data } = useTypedQuery(['articles', offset, limit], ArticlesQuery, {}, {
    variables: { limit, offset },
  });

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
