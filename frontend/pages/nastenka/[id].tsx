import React from 'react';
import { AnnouncementList } from '@/ui/lists/AnnouncementList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';
import { AnnouncementDocument } from '@/graphql/Announcement';
import { useQuery } from 'urql';
import { AnnouncementItem } from '@/ui/AnnouncementItem';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export default function AnnouncementPage() {
  const router = useTypedRouter(QueryParams);
  const { id } = router.query
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
