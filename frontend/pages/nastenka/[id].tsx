import React from 'react';
import { AnnouncementList } from '@app/ui/AnnouncementList';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';
import { AnnouncementDocument } from '@/graphql/Announcement';
import { useQuery } from 'urql';
import { AnnouncementItem } from '@/ui/AnnouncementItem';

function Page() {
  const id = fromSlugArray(useRouter().query.id);
  const [query] = useQuery({ query: AnnouncementDocument, variables: { id }, pause: !id });
  const data = query.data?.upozorneni;

  return (
    <Layout requireMember>
      <NextSeo title="Nástěnka" />
      <WithSidebar sidebar={<AnnouncementList />}>
        {data ? <AnnouncementItem item={data} /> : null}
      </WithSidebar>
    </Layout>
  );
}

export default Page;
