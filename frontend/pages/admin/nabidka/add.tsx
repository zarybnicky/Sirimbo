import { ReservationForm } from 'components/ReservationForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { ReservationList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <ReservationForm />;

Page.list = <ReservationList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNabidka, PermissionLevel.P_OWNED];
Page.staticTitle = "Nabídky";

export default Page;
