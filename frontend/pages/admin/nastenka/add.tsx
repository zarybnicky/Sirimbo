import { AnnouncementForm } from 'components/AnnouncementForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { AnnouncementList } from 'lib/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <AnnouncementForm />;

Page.list = <AnnouncementList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_OWNED];
Page.staticTitle = "Nástěnka";

export default Page;
