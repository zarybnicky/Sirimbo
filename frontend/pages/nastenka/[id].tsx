import React from 'react';
import { AnnouncementList } from '@app/ui/AnnouncementList';
import { AnnouncementForm } from '@app/ui/AnnouncementForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';
import { AnnouncementDocument } from '@/graphql/Announcement';
import { useQuery } from 'urql';

function Page() {
  const id = fromSlugArray(useRouter().query.id);
  const [query] = useQuery({ query: AnnouncementDocument, variables: { id }, pause: !id });
  const data = query.data?.upozorneni;

  return (
    <Layout requireTrainer>
      <NextSeo title="Nástěnka" />
      <WithSidebar sidebar={<AnnouncementList />}>
        {data ? <AnnouncementForm id={id} data={data} /> : null}
      </WithSidebar>
    </Layout>
  );
}

export default Page;
