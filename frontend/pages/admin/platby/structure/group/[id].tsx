import { PaymentGroupForm } from 'components/PaymentGroupForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from 'lib/slugify';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentGroupList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <PaymentGroupForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <PaymentGroupList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
