import * as React from 'react';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { CoupleList } from 'components/CoupleList';
import { Layout } from 'components/layout/Layout';

export default function CoupleAdminList() {
  return null;
}

CoupleAdminList.getLayout = (page: React.ReactElement) => (
  <Layout list={<CoupleList />}>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePary,
  PermissionLevel.P_OWNED,
);
