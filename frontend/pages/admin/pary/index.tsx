import * as React from 'react';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { CoupleList } from 'components/CoupleList';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <CoupleList />;

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePary,
  PermissionLevel.P_OWNED,
);
