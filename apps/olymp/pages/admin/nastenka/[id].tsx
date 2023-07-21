import { AnnouncementForm } from '@app/ui/AnnouncementForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { AnnouncementList } from '@app/ui/entity-lists';
import { WithEntity } from '@app/ui/generic/WithEntity';
import { Announcement } from '@app/ui/entities';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';
import { WithSidebar } from '@app/ui/WithSidebar';

const Page = () => (
  <Layout permissions={[PermissionKey.peNastenka, PermissionLevel.P_OWNED]}>
    <NextSeo title="Nástěnka" />
    <WithSidebar sidebar={<AnnouncementList />}>
      <WithEntity
        perms={[PermissionKey.peNastenka, PermissionLevel.P_OWNED]}
        fetcher={AnnouncementForm.fetcher}
        id={fromSlugArray(useRouter().query.id)}
      >
        <AnnouncementForm entity={Announcement} />
      </WithEntity>
    </WithSidebar>
  </Layout>
);

export default Page;
