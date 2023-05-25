import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { CohortGroupList } from 'lib/entity-lists';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <CohortGroupList />;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Tréninkové programy";

export default Page;
