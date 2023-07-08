import { CohortGroupForm } from '@app/ui/CohortGroupForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { CohortGroupList } from '@app/ui/entity-lists';
import { CohortGroup } from '@app/ui/entities';

const Page: NextPageWithLayout = () => <CohortGroupForm entity={CohortGroup} />;

Page.list = <CohortGroupList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Tréninkové programy";

export default Page;
