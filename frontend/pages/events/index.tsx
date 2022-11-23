import * as React from 'react';
import { HtmlView } from 'components/HtmlView';
import classNames from 'classnames';
import { Card } from 'components/Card';
import { EventWithItemsFragment, useEventListQuery } from 'lib/graphql/Event';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { fullDateFormatter } from 'lib/format-date';
import { useRouter } from 'next/router';

const EventItem = ({ event }: { event: EventWithItemsFragment }) => {
  const router = useRouter()
  const [expanded, setExpanded] = React.useState(false);
  const open = React.useCallback(() => {
    if (!expanded) {
      setExpanded(true);
    } else {
      router.push(`/event/${event.id}`);
    }
  }, [expanded]);

  return (
    <Card onClick={open} className={classNames("break-inside-avoid cursor-pointer")}>
      <div className="text-lg text-stone-600">
        {fullDateFormatter.formatRange(new Date(event.aOd), new Date(event.aDo))}
      </div>
      <div className="text-4xl text-stone-800">{event.aJmeno}</div>

      <HtmlView
        className={expanded ? "" : "line-clamp-3 after"}
        content={event.aInfo.replaceAll('\n', '<br/>')}
      />
      {!expanded && (
        <div className="text-red-500 font-bold mt-3">Zobrazit více...</div>
      )}
    </Card>
  );
};

export default function EventListPage() {
  const { data } = useEventListQuery({ visible: true });

  return <Item>
    <Item.Titlebar title="Nadcházející události" />
    {data?.akces?.nodes.map(event => <EventItem key={event.id} event={event} />)}
  </Item>;
}

export const getServerSideProps = withServerPermissions(PermissionKey.peAkce, PermissionLevel.P_MEMBER);
