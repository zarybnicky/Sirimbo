import { CohortsList } from 'components/CohortList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => null;

Page.list = <CohortsList />;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
