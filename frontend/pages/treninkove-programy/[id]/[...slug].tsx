import { CohortGroupDocument, CohortGroupFragment } from '@app/graphql/CohortGroup';
import { fetchGql } from '@app/graphql/query';
import { TitleBar } from '@app/ui/TitleBar';
import { RichTextView } from '@app/ui/RichTextView';
import { fromSlugArray, slugify } from '@app/ui/slugify';
import { Layout } from '@/components/layout/Layout';
import { GetStaticProps } from 'next';
import React from 'react';
import { Card } from '@app/ui/Card';
import Link from 'next/link';

type PageProps = {
  item: CohortGroupFragment;
};

const Page: React.FC<PageProps> = ({ item }) => {
  return (
    <Layout hideTopMenuIfLoggedIn>
      <TitleBar title={item.name} />
      <div className="container py-4">
        <RichTextView className="mb-10" value={item.description} />
        {item.cohorts.nodes.map((item) => (
          <Card key={item.id} cohort={item} className="group break-inside-avoid">
            <h5 className="text-xl underline">
              <Link href={`/treninkove-skupiny/${item.id}`}>{item.sName}</Link>
            </h5>
            <h6 className="font-bold mb-2">{item.sLocation}</h6>
            <RichTextView
              value={item.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
            />
          </Card>
        ))}
      </div>
    </Layout>
  );
};

export default Page;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const id = fromSlugArray(context.params?.id) || fromSlugArray(context.params?.slug);
  const item = await fetchGql(CohortGroupDocument, { id }).then((x) => x.cohortGroup);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const slug = slugify(item.name);
  if (fromSlugArray(context.params?.slug || '') !== slug) {
    return {
      revalidate: 60,
      redirect: {
        destination: `/treninkove-programy/${item.id}/${slug}`,
        permanent: false,
      },
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
