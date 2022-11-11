import * as React from 'react';
import { EventWithItemsFragment } from 'lib/graphql';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { useEventListQuery } from "lib/graphql";
import { HtmlView } from 'components/HtmlView';
import classNames from 'classnames';

const EventItem = ({ event }: { event: EventWithItemsFragment }) => {
  const [expanded, setExpanded] = React.useState(false);
  const open = React.useCallback(() => setExpanded(true), []);

  return (
    <div onClick={open} className={classNames(
      "bg-white overflow-hidden border border-slate-100 shadow-lg sm:rounded-lg p-4 mb-2 break-inside-avoid",
      !expanded && 'cursor-pointer',
    )}>
      <div className="text-4xl text-gray-600">{event.aJmeno}</div>

      <div className="flex gap-4">
        <div>
          <HtmlView
            className={expanded ? "" : "line-clamp-3 after"}
            content={event.aInfo.replaceAll('\n', '<br/>')}
          />
          {!expanded && (
            <div className="text-primary font-bold mt-3">Zobrazit více...</div>
          )}
        </div>
        {expanded && (
          <div>
            <div className="text-slate font-bold mb-2">Účastníci</div>
            {event.akceItemsByAiIdRodic.nodes.map((item) => (
              <div>{item.userByAiUser?.uJmeno} {item.userByAiUser?.uPrijmeni}</div>
            ))}
          </div>
        )}
      </div >
    </div>
  );
};

export default function EventListPage() {
  useRequireUserLoggedIn();
  const { data } = useEventListQuery({ visible: true });

  return <div className="container max-w-6xl mx-auto mt-4 mb-8">
    {data?.akces?.nodes.map(event => <EventItem key={event.aId} event={event} />)}
  </div >;
}
