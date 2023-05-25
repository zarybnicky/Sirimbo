import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { CohortList } from 'lib/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <CohortList />;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
