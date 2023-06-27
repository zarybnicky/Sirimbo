import { ReservationForm } from '@app/ui/ReservationForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { ReservationList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <ReservationForm id={fromSlugArray(useRouter().query.id)}/>;

Page.list = <ReservationList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNabidka, PermissionLevel.P_OWNED];
Page.staticTitle = 'Nabídky';

export default Page;
