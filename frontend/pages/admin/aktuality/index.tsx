import * as React from 'react';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { ArticleList } from 'lib/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <ArticleList />
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Aktuality";

export default Page;
