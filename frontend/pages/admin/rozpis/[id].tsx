import { ScheduleForm } from 'components/ScheduleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { ScheduleList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <ScheduleForm id={fromSlugArray(useRouter().query.id)}/>;

Page.list = <ScheduleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_OWNED];
Page.staticTitle = 'Rozpisy';

export default Page;
