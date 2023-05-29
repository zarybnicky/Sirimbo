import { EventForm } from 'components/EventForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { EventList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <EventForm />;

Page.list = <EventList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAkce, PermissionLevel.P_OWNED];
Page.staticTitle = "Akce";

export default Page;
