import { AnnouncementForm } from 'components/AnnouncementForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import { NextPageWithLayout } from 'pages/_app';
import { AnnouncementList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <AnnouncementForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <AnnouncementList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_OWNED];
Page.staticTitle = 'Nástěnka';

export default Page;
