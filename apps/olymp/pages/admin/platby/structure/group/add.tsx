import { PaymentGroupForm } from '@app/ui/PaymentGroupForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentGroupList } from '@app/ui/entity-lists';
import { PaymentGroup } from '@app/ui/entities';

const Page: NextPageWithLayout = () => <PaymentGroupForm entity={PaymentGroup} />;

Page.list = <PaymentGroupList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = "Platby";

export default Page;
