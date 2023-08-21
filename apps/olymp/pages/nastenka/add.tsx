import { AnnouncementForm } from '@app/ui/AnnouncementForm';
import { AnnouncementList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout requireMember>
    <NextSeo title="Nástěnka" />
    <WithSidebar sidebar={<AnnouncementList />}>
      <AnnouncementForm />
    </WithSidebar>
  </Layout>
);

export default Page;
