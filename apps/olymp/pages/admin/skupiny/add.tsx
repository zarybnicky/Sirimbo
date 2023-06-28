import { CohortForm } from '@app/ui/CohortForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Cohort } from 'lib/entities';
import { CohortList } from 'lib/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <CohortForm entity={Cohort} />

Page.list = <CohortList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
