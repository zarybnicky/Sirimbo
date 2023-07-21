import { CohortForm } from '@app/ui/CohortForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Cohort } from '@app/ui/entities';
import { CohortList } from '@app/ui/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <CohortForm entity={Cohort} />

Page.list = <CohortList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
