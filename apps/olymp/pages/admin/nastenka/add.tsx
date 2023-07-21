import { AnnouncementForm } from '@app/ui/AnnouncementForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Announcement } from '@app/ui/entities';
import { AnnouncementList } from '@app/ui/entity-lists';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peNastenka, PermissionLevel.P_OWNED]}>
    <NextSeo title="Nástěnka" />
    <WithSidebar sidebar={<AnnouncementList />}>
      <AnnouncementForm entity={Announcement} />
    </WithSidebar>
  </Layout>
);

export default Page;
