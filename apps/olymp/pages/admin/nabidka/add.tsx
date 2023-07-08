import { ReservationForm } from '@app/ui/ReservationForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { ReservationList } from '@app/ui/entity-lists';
import { Reservation } from '@app/ui/entities';

const Page: NextPageWithLayout = () => <ReservationForm entity={Reservation} />;

Page.list = <ReservationList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peNabidka, PermissionLevel.P_OWNED];
Page.staticTitle = "Nabídky";

export default Page;
