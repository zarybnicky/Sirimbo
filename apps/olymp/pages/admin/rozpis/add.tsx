import { ScheduleForm } from '@app/ui/ScheduleForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { ScheduleList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <ScheduleForm />;

Page.list = <ScheduleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_OWNED];
Page.staticTitle = "Rozpisy";

export default Page;
