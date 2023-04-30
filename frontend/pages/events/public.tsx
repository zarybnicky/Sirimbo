import * as React from 'react';
import { usePublicEventsQuery } from 'lib/graphql/Event';
import { Item } from 'components/layout/Item';
import { EventItem } from 'components/EventItem';
import { Layout } from 'components/layout/Layout';

export default function PublicEventListPage() {
  const { data } = usePublicEventsQuery();

  return (
    <Item className="feature p-2">
      <Item.Titlebar title="Nadcházející akce" />
      {data?.events?.nodes.map((event) => (
        <EventItem key={event.id} event={event} />
      ))}
    </Item>
  );
}

PublicEventListPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
