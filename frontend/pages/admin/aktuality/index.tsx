import * as React from 'react';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { ArticleList } from 'components/ArticleList';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <ArticleList />
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];

export default Page;
