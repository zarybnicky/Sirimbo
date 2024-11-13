import { AnnouncementList } from '@/ui/lists/AnnouncementList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';

export default function AnnouncementsPage() {
  return (
    <Layout requireMember>
      <NextSeo title="Nástěnka" />
      <WithSidebar sidebar={<AnnouncementList />} />
    </Layout>
  );
}
