import { getPlaceholder } from '../test-utils';
import { $, Selector, AktualitiesOrderBy } from 'lib/zeus';
import { useTypedQuery } from 'lib/zeus/apollo';

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
  const { data } = useTypedQuery(ArticlesQuery, {
    apolloOptions: {
      variables: { limit, offset },
    },
  });

  return {
    articles: (data?.aktualities?.nodes || []).map(x => ({
      href: `/articles/${x.atId}`,
      img: `/galerie/${x.galerieFotoByAtFotoMain?.gfPath}`,
      imgThumb: `/galerie/thumbnails/${x.galerieFotoByAtFotoMain?.gfPath}`,
      header: x.atJmeno,
      preview: x.atPreview,
    })),
    count: data?.aktualities?.totalCount || 0,
  };
};

export const useMockArticles = (): Article[] => [
  {
    href: "/o-nas",
    img: getPlaceholder(1024, 768),
    imgThumb: getPlaceholder(360, 240),
    header: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
  {
    href: "/o-nas",
    img: getPlaceholder(1024, 768),
    imgThumb: getPlaceholder(360, 240),
    header: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
  {
    href: "/o-nas",
    img: getPlaceholder(1024, 768),
    imgThumb: getPlaceholder(360, 240),
    header: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
  {
    href: "/o-nas",
    img: getPlaceholder(1024, 768),
    imgThumb: getPlaceholder(360, 240),
    header: "Přípravný kurz tanečního sportu",
    preview: "Otevíráme kurz pro mládež a dospělé v Olomouci a Přerově"
  },
];