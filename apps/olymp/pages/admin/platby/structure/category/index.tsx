import * as React from 'react';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentCategoryList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => null;

Page.list = <PaymentCategoryList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
