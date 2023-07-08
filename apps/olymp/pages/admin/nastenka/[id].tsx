import { AnnouncementForm } from '@app/ui/AnnouncementForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import { NextPageWithLayout } from 'pages/_app';
import { AnnouncementList } from '@app/ui/entity-lists';
import { WithEntity } from '@app/ui/generic/WithEntity';
import { Announcement } from '@app/ui/entities';

const Page: NextPageWithLayout = () => (
  <WithEntity
    perms={[PermissionKey.peNastenka, PermissionLevel.P_OWNED]}
    fetcher={AnnouncementForm.fetcher}
    id={fromSlugArray(useRouter().query.id)}
  >
    <AnnouncementForm entity={Announcement} />
  </WithEntity>
);

Page.list = <AnnouncementList />;
Page.isDetail = true;
Page.staticTitle = 'Nástěnka';

export default Page;
