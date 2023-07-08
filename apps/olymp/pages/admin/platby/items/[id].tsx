import { PaymentItemForm } from '@app/ui/PaymentItemForm';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { PaymentItemList } from '@app/ui/entity-lists';
import { PaymentItem } from '@app/ui/entities';

const Page: NextPageWithLayout = () => <PaymentItemForm entity={PaymentItem} id={fromSlugArray(useRouter().query.id)} />;

Page.list = <PaymentItemList />;
Page.permissions = [PermissionKey.pePlatby, PermissionLevel.P_OWNED];
Page.staticTitle = 'Platby';

export default Page;
