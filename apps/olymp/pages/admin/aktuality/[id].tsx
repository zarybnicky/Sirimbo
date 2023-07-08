import { ArticleForm } from '@app/ui/ArticleForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from '@app/ui/slugify';
import { ArticleList } from '@app/ui/entity-lists';
import { Article } from '@app/ui/entities';

const Page: NextPageWithLayout = () => <ArticleForm entity={Article} id={fromSlugArray(useRouter().query.id)} />;

Page.list = <ArticleList />;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Aktuality";
Page.isDetail = true;

export default Page;
