import * as React from 'react';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { ReservationList } from 'components/ReservationList';
import { Layout } from 'components/layout/Layout';

export default function ReservationListPage() {
  return null;
}

ReservationListPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<ReservationList />}>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNabidka, PermissionLevel.P_VIEW,
);
