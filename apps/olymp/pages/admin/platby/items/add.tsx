import { PaymentItemForm } from '@app/ui/PaymentItemForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentItemList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <PaymentItemForm />;

Page.list = <PaymentItemList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = "Platby";

export default Page;
