import { EventForm } from '@app/ui/EventForm';
import { WithSidebar } from '@app/ui/WithSidebar';
import { Event } from '@app/ui/entities';
import { EventList } from '@app/ui/entity-lists';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { Layout } from 'components/layout/Layout';
import { NextSeo } from 'next-seo';

const Page = () => (
  <Layout permissions={[PermissionKey.peAkce, PermissionLevel.P_OWNED]}>
    <NextSeo title="Akce" />
    <WithSidebar sidebar={<EventList />}>
      <EventForm entity={Event} />
    </WithSidebar>
  </Layout>
);

export default Page;
