import { CohortForm } from 'components/CohortForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { CohortList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <CohortForm />

Page.list = <CohortList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
