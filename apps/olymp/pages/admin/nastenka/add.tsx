import { AnnouncementForm } from '@app/ui/AnnouncementForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { AnnouncementList } from 'lib/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <AnnouncementForm />;

Page.list = <AnnouncementList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_OWNED];
Page.staticTitle = "Nástěnka";

export default Page;
