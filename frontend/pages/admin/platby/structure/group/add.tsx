import { PaymentGroupForm } from 'components/PaymentGroupForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentGroupList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <PaymentGroupForm />;

Page.list = <PaymentGroupList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = "Platby";

export default Page;
