import { CohortGroupForm } from '@app/ui/CohortGroupForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fromSlugArray } from '@app/ui/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { CohortGroupList } from '@app/ui/entity-lists';
import { CohortGroup } from '@app/ui/entities';

const Page: NextPageWithLayout = () => <CohortGroupForm entity={CohortGroup} id={fromSlugArray(useRouter().query.id)} />;

Page.list = <CohortGroupList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = 'Tréninkové programy';

export default Page;
