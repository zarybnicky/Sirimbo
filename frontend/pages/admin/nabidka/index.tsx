import * as React from 'react';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { ReservationList } from 'components/ReservationList';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <ReservationList />;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNabidka,
  PermissionLevel.P_VIEW,
);
