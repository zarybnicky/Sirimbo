import * as React from 'react';
import { CallToAction } from 'components/CallToAction';
import { useRouter } from 'next/router';
import { ArticleFragment, useArticleQuery } from 'lib/graphql/Articles';
import { fullDateFormatter } from 'lib/format-date';
import { RichTextView } from 'components/RichTextView';
import { NextSeo } from 'next-seo';
import { Heading } from 'components/Heading';
import { type NextPageWithLayout } from 'pages/_app';
import { GetStaticProps } from 'next';
import { fromSlugArray, slugify } from 'lib/slugify';

type PageProps = {
  item: ArticleFragment;
};

const Page: NextPageWithLayout<PageProps> = ({ item }) => {
  const router = useRouter();

  return (
    <>
      <NextSeo
        title={item.atJmeno}
        openGraph={{
          type: 'article',
          title: item.atJmeno,
          url: 'https://tkolymp.cz/articles/${item.id}/${slugify(item.atJmeno)}',
          images: [{ url: 'https://tkolymp.cz/galerie/thumbnails/{item.atFotoMain}' }],
          description: item.atPreview,
        }}
      />
      <Heading>{item.atJmeno}</Heading>
      <div className="text-stone-600 mb-6">
        {item.atTimestampAdd && fullDateFormatter.format(new Date(item.atTimestampAdd))}
      </div>
      <RichTextView value={item.atText} />
      <CallToAction />
    </>
  );
};

Page.showTopMenu = true;

export default Page;

export const getStaticPaths = async () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const id = fromSlugArray(context.params?.id) || fromSlugArray(context.params?.slug);
  const item = await useArticleQuery
    .fetcher({ id })()
    .then(
      (query) => query.aktuality,
      () => null,
    );

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  if (fromSlugArray(context.params?.slug || '') !== slugify(`${item?.atJmeno}`)) {
    return {
      revalidate: 60,
      redirect: {
        destination: `/articles/${item.id}/${slugify(item.atJmeno)}`,
        permanent: false,
      },
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
