import { AnnouncementForm } from '@app/ui/AnnouncementForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Announcement } from '@app/ui/entities';
import { AnnouncementList } from '@app/ui/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <AnnouncementForm entity={Announcement} />;

Page.list = <AnnouncementList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_OWNED];
Page.staticTitle = "Nástěnka";

export default Page;
