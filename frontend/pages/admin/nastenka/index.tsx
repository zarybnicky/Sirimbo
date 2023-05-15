import { AnnouncementList } from 'components/AnnouncementList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <AnnouncementList />;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];

export default Page;
