import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { ScheduleList } from '@app/ui/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <ScheduleList />
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_OWNED];
Page.staticTitle = "Rozpisy";

export default Page;
