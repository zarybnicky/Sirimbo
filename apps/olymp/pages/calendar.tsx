import React from 'react';
import { Calendar } from '@app/calendar/Calendar';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () =>
    process.env.NEXT_PUBLIC_OLD_STYLE_LAYOUT
    ? <div className="min-h-[80vh] col-feature"><Calendar /></div>
    : <Calendar />;

Page.staticTitle = "Nástěnka";
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
