import * as React from 'react';
import { ReservationList } from 'components/ReservationList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <ReservationList />;
Page.permissions = [PermissionKey.peNabidka, PermissionLevel.P_OWNED];

export default Page;
