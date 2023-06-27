import { PaymentGroupForm } from '@app/ui/PaymentGroupForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentGroupList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <PaymentGroupForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <PaymentGroupList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
