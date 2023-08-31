import { AnnouncementList } from '@app/ui/AnnouncementList';
import { Layout } from '@/components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireTrainer>
    <NextSeo title="Nástěnka" />
    <WithSidebar sidebar={<AnnouncementList />} />
  </Layout>
);

export default Page;
