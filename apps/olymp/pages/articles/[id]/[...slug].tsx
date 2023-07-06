import { CallToAction } from '@app/branding-olymp/CallToAction';
import { RichTextView } from '@app/editor/RichTextView';
import { ArticleDocument, ArticleFragment } from '@app/graphql/Articles';
import { fetchGql } from '@app/graphql/query';
import { Heading } from '@app/ui/Heading';
import { fullDateFormatter } from '@app/ui/format-date';
import { fromSlugArray, slugify } from '@app/ui/slugify';
import { GetStaticProps } from 'next';
import { NextSeo } from 'next-seo';
import type { NextPageWithLayout } from 'pages/_app';
import * as React from 'react';

type PageProps = {
  item: ArticleFragment;
};

const Page: NextPageWithLayout<PageProps> = ({ item }) => {

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
      <div className="text-neutral-11 mb-6 -mt-4">
        {item.atTimestampAdd && fullDateFormatter.format(new Date(item.atTimestampAdd))}
      </div>
      <RichTextView value={item.atText} />
      <CallToAction />
    </>
  );
};

Page.showTopMenu = true;

export default Page;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const id = fromSlugArray(context.params?.id) || fromSlugArray(context.params?.slug);
  const item = await fetchGql(ArticleDocument, {id}).then(x => x.aktuality);

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
