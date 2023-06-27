import { PaymentCategoryForm } from '@app/ui/PaymentCategoryForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentCategoryList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <PaymentCategoryForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <PaymentCategoryList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
