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
    <Item className="col-full-width p-2 bg-stone-100">
      <Item.Titlebar title="Nadcházející akce" />
      <div className="grid grid-cols-2 gap-2">
        {data?.events?.nodes.map((event) => (
          <EventItem key={event.id} event={event} />
        ))}
      </div>
    </Item>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce,
  PermissionLevel.P_MEMBER,
);
