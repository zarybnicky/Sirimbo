import { CohortGroupForm } from 'components/CohortGroupForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { CohortGroupList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <CohortGroupForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <CohortGroupList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = 'Tréninkové programy';

export default Page;
