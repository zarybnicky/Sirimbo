import { ArticleForm } from 'components/ArticleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';
import { ArticleList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <ArticleForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <ArticleList />;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Aktuality";
Page.isDetail = true;

export default Page;
