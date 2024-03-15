import { AnnouncementList } from '@/ui/AnnouncementList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@/ui/WithSidebar';

const Page = () => (
  <Layout requireMember>
    <NextSeo title="Nástěnka" />
    <WithSidebar sidebar={<AnnouncementList />} />
  </Layout>
);

export default Page;
