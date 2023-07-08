import { EventForm } from '@app/ui/EventForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { EventList } from '@app/ui/entity-lists';
import { Event } from '@app/ui/entities';

const Page: NextPageWithLayout = () => <EventForm entity={Event} />;

Page.list = <EventList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAkce, PermissionLevel.P_OWNED];
Page.staticTitle = "Akce";

export default Page;
