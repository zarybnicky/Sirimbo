import { PaymentCategoryForm } from 'components/PaymentCategoryForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentCategoryList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <PaymentCategoryForm />;

Page.list = <PaymentCategoryList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = "Platby";

export default Page;
