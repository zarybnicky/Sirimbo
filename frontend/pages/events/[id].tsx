import * as React from 'react';
import { useRouter } from 'next/router';
import { HtmlView } from 'components/HtmlView';
import { fullDateFormatter } from 'lib/format-date';
import { Layout } from 'components/layout/Layout';
import { Card } from 'components/Card';
import { useEventQuery } from 'lib/graphql/Event';

export default function EventPage() {
  const router = useRouter();
  const id = router.query.id as string;
  const { data } = useEventQuery({ id });
  const event = data?.akce;
  if (!event) {
    return null;
  }

  return (
    <Card>
      <div className="text-lg text-stone-600">
        {fullDateFormatter.formatRange(new Date(event.aOd), new Date(event.aDo))}
      </div>
      <div className="text-4xl text-gray-600">{event.aJmeno}</div>

      <HtmlView content={event.aInfo.replaceAll('\n', '<br/>')} />

      <div className="text-stone-700 font-bold mb-2">Účastníci</div>
      {event.akceItemsByAiIdRodic.nodes.map((item) => (
        <div key={item.id}>{item.userByAiUser?.uJmeno} {item.userByAiUser?.uPrijmeni}</div>
      ))}
    </Card>
  );
};


EventPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
