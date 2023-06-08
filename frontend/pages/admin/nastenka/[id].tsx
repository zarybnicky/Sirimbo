import { AnnouncementForm } from 'components/AnnouncementForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import { NextPageWithLayout } from 'pages/_app';
import { AnnouncementList } from 'lib/entity-lists';
import { WithEntity } from 'components/generic/WithEntity';

const Page: NextPageWithLayout = () => (
  <WithEntity
    perms={[PermissionKey.peNastenka, PermissionLevel.P_OWNED]}
    fetcher={AnnouncementForm.fetcher}
    id={fromSlugArray(useRouter().query.id)}
  >
    {AnnouncementForm}
  </WithEntity>
);

Page.list = <AnnouncementList />;
Page.isDetail = true;
Page.staticTitle = 'Nástěnka';

export default Page;
