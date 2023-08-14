import { CohortDocument, CohortFragment } from '@app/graphql/Cohorts';
import { fetchGql } from '@app/graphql/query';
import { CohortItem } from '@app/ui/CohortItem';
import { Heading } from '@app/ui/Heading';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from 'components/layout/Layout';
import { GetStaticProps } from 'next';
import { NextSeo } from 'next-seo';
import React from 'react';

type PageProps = {
  item: CohortFragment;
};

const Page: React.FC<PageProps> = ({ item }) => {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={item.sName} />
      <Heading>{item.sName}</Heading>
      <div className="container">
        <CohortItem id={item.id} />
      </div>
    </Layout>
  );
};

export default Page;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const id = fromSlugArray(context.params?.id) || fromSlugArray(context.params?.slug);
  const item = await fetchGql(CohortDocument, { id }).then((x) => x.entity);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
