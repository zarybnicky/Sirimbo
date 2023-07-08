import { ScheduleForm } from '@app/ui/ScheduleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { ScheduleList } from '@app/ui/entity-lists';
import { Schedule } from '@app/ui/entities';

const Page: NextPageWithLayout = () => <ScheduleForm entity={Schedule} id={fromSlugArray(useRouter().query.id)}/>;

Page.list = <ScheduleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_OWNED];
Page.staticTitle = 'Rozpisy';

export default Page;
