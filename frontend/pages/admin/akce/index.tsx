import { EventList } from 'components/EventList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <EventList />;
Page.permissions = [PermissionKey.peAkce, PermissionLevel.P_OWNED];
Page.staticTitle = "Akce";

export default Page;
