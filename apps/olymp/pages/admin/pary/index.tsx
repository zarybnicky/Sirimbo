import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { CoupleList } from '@app/ui/entity-lists';

const Page: NextPageWithLayout = () => null;

Page.list = <CoupleList />;
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];
Page.staticTitle = "Páry";

export default Page;
