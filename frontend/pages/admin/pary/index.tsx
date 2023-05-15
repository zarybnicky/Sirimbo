import * as React from 'react';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { CoupleList } from 'components/CoupleList';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <CoupleList />;
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];

export default Page;
