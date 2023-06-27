import { CohortForm } from '@app/ui/CohortForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { CohortList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <CohortForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <CohortList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
