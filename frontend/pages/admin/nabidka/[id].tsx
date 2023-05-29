import { ReservationForm } from 'components/ReservationForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { ReservationList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <ReservationForm id={fromSlugArray(useRouter().query.id)}/>;

Page.list = <ReservationList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNabidka, PermissionLevel.P_OWNED];
Page.staticTitle = 'Nabídky';

export default Page;
