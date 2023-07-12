import { CoupleView } from '@app/ui/CoupleView';
import { CoupleList } from '@app/ui/entity-lists';
import { fromSlugArray } from '@app/ui/slugify';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { useRouter } from 'next/router';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => <CoupleView id={fromSlugArray(useRouter().query.id)} />;

Page.list = <CoupleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];
Page.staticTitle = 'Páry';

export default Page;
