import { ArticleForm } from '@app/ui/ArticleForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { ArticleList } from 'lib/entity-lists';
import { Article } from 'lib/entities';

const Page: NextPageWithLayout = () => <ArticleForm entity={Article} />;

Page.list = <ArticleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Aktuality";

export default Page;
