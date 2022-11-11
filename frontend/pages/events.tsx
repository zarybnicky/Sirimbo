import * as React from 'react';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { useEventListQuery } from "lib/graphql";
import { HtmlView } from 'components/HtmlView';

export default function EventListPage() {
  useRequireUserLoggedIn();
  const { data } = useEventListQuery({ visible: true });

  return <div className="container max-w-6xl mx-auto mt-4 mb-8">
    {data?.akces?.nodes.map(event => (
      <div key={event.aId} className="bg-white overflow-hidden border border-slate-100 shadow-lg sm:rounded-lg p-4 mb-2 break-inside-avoid">
        <div className="text-4xl text-gray-600">{event.aJmeno}</div>

        <HtmlView content={event.aInfo.replaceAll('\n', '<br/>')} />
        <div className="flex flex-col items-start">
          {event.akceItemsByAiIdRodic.nodes.map((item) => (
            <>{item.userByAiUser?.uJmeno} {item.userByAiUser?.uPrijmeni}</>
          ))}
        </div>
      </div>
    ))
    }
  </div >;
}
