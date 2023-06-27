import { EventForm } from '@app/ui/EventForm';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from '@app/ui/slugify';
import { EventList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => <EventForm id={fromSlugArray(useRouter().query.id)} />;

Page.list = <EventList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAkce, PermissionLevel.P_OWNED];
Page.staticTitle = 'Akce';

export default Page;
