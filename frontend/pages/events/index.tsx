import * as React from 'react';
import { useMyEventsQuery } from 'lib/graphql/Event';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { EventItem } from 'components/EventItem';

export default function EventListPage() {
  const { data } = useMyEventsQuery();

  return (
    <Item className="col-full-width">
      <Item.Titlebar title="Nadcházející akce" />
      {data?.akces?.nodes.map((event) => (
        <EventItem key={event.id} event={event} />
      ))}
    </Item>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce,
  PermissionLevel.P_MEMBER,
);
