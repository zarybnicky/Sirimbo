import * as React from 'react';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { CoupleList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => null;

Page.list = <CoupleList />;
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];
Page.staticTitle = "Páry";

export default Page;
