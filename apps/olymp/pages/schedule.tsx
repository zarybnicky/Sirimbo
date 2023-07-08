import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { ScheduleView } from '@app/ui/ScheduleView';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <ScheduleView />;

Page.staticTitle = "Rozpis";
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_MEMBER];

export default Page;
