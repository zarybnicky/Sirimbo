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
        {item.cohortsList.map((item) => (
          <Card key={item.id} className="group break-inside-avoid pl-8">
            <h5 className="text-xl underline">
              <Link href={`/treninkove-skupiny/${item.id}`}>{item.name}</Link>
            </h5>
            <h6 className="font-bold mb-2">{item.location}</h6>
            <RichTextView
              value={item.description.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
            />
            <div
              className="absolute rounded-l-lg w-4 border-r border-neutral-6 shadow-sm inset-y-0 left-0"
              style={{ backgroundColor: item.colorRgb }}
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
  const params = QueryParams.parse(context.params);
  let { id } = params
  if (!id) {
    id = params.slug;
  }
  const item = await fetchGql(CohortGroupDocument, { id }).then((x) => x.cohortGroup);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const expectedSlug = slugify(item.name);
  if (params.slug !== expectedSlug) {
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
