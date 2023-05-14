import * as React from 'react';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { ArticleList } from 'components/ArticleList';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <ArticleList />

export default Page;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAktuality,
  PermissionLevel.P_OWNED,
);
