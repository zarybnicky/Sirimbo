import { PaymentCategoryForm } from 'components/PaymentCategoryForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentCategoryList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <PaymentCategoryForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <PaymentCategoryList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
