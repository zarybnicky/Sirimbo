import * as React from 'react';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { ReservationList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => null;

Page.list = <ReservationList />;
Page.permissions = [PermissionKey.peNabidka, PermissionLevel.P_OWNED];
Page.staticTitle = "Nabídky";

export default Page;
