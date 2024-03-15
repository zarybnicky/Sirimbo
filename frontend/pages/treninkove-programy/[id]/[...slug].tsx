import { CohortGroupDocument, CohortGroupFragment } from '@/graphql/CohortGroup';
import { fetchGql } from '@/graphql/query';
import { TitleBar } from '@/ui/TitleBar';
import { RichTextView } from '@/ui/RichTextView';
import { slugify } from '@/ui/slugify';
import { Layout } from '@/components/layout/Layout';
import { GetStaticProps } from 'next';
import React from 'react';
import { Card } from '@/ui/Card';
import Link from 'next/link';
import { z } from 'zod';
import { zRouterString } from '@/ui/useTypedRouter';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

type PageProps = {
  item: CohortGroupFragment;
};

function TrainingGroupPage({ item }: PageProps) {
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

export default TrainingGroupPage;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  let { id, slug } = QueryParams.parse(context.params);
  if (!id) {
    id = slug;
  }
  const item = await fetchGql(CohortGroupDocument, { id }).then((x) => x.cohortGroup);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const expectedSlug = slugify(item.name);
  if (slug !== expectedSlug) {
    return {
      revalidate: 60,
      redirect: {
        destination: `/treninkove-programy/${item.id}/${expectedSlug}`,
        permanent: false,
      },
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};
