import { ArticleForm } from 'components/ArticleForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { ArticleList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <ArticleForm />;

Page.list = <ArticleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Aktuality";

export default Page;
