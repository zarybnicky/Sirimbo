import React from 'react';
import { Calendar } from '@app/calendar/Calendar';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <Calendar />;

Page.staticTitle = "Nástěnka";
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
