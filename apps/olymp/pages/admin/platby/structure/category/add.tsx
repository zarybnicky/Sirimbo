import { PaymentCategoryForm } from '@app/ui/PaymentCategoryForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentCategoryList } from 'lib/entity-lists';
import { PaymentCategory } from 'lib/entities';

const Page: NextPageWithLayout = () => <PaymentCategoryForm entity={PaymentCategory} />;

Page.list = <PaymentCategoryList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = "Platby";

export default Page;