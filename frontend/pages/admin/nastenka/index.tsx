import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { AnnouncementList } from 'lib/entity-lists';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <AnnouncementList />;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Nástěnka";

export default Page;
