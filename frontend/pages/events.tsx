import * as React from 'react';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { HtmlView } from 'components/HtmlView';
import classNames from 'classnames';
import { Card } from 'components/Card';
import { EventWithItemsFragment, useEventListQuery } from 'lib/graphql/Event';
import { Layout } from 'components/layout/Layout';

const EventItem = ({ event }: { event: EventWithItemsFragment }) => {
  const [expanded, setExpanded] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);

  return (
    <Card onClick={open} className={classNames("break-inside-avoid", !expanded && 'cursor-pointer')}>
      <div className="text-4xl text-gray-600">{event.aJmeno}</div>

      <div className="flex gap-4">
        <div>
          <HtmlView
            className={expanded ? "" : "line-clamp-3 after"}
            content={event.aInfo.replaceAll('\n', '<br/>')}
          />
          {!expanded && (
            <div className="text-red-500 font-bold mt-3">Zobrazit více...</div>
          )}
        </div>
        {expanded && (
          <div>
            <div className="text-slate font-bold mb-2">Účastníci</div>
            {event.akceItemsByAiIdRodic.nodes.map((item) => (
              <div key={item.id}>{item.userByAiUser?.uJmeno} {item.userByAiUser?.uPrijmeni}</div>
            ))}
          </div>
        )}
      </div >
    </Card>
  );
};

export default function EventListPage() {
  useRequireUserLoggedIn();
  const { data } = useEventListQuery({ visible: true });

  return <div className="container mx-auto max-w-5xl mt-4 mb-8">
    {data?.akces?.nodes.map(event => <EventItem key={event.id} event={event} />)}
  </div >;
}

EventListPage.getLayout = (page: React.ReactElement) => (
  <Layout withBleeds>{page}</Layout>
);
