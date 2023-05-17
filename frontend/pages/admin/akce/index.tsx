import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { EventList } from 'lib/entity-lists';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <EventList />;
Page.permissions = [PermissionKey.peAkce, PermissionLevel.P_OWNED];
Page.staticTitle = "Akce";

export default Page;
