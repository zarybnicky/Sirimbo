import * as React from 'react';
import { useEventQuery, useMyEventsQuery } from 'lib/graphql/Event';
import { Item } from 'components/layout/Item';
import { EventItem } from 'components/EventItem';
import { useAuth } from 'lib/data/use-auth';
import { Layout } from 'components/layout/Layout';
import { EventMemberList } from 'components/EventMemberList';
import { useRouter } from 'next/router';

export default function EventItemPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useEventQuery({ id: id as string }, { enabled: !!id });
  return (
    <Item className="col-full-width p-2 bg-stone-100">
      <Item.Titlebar title="Nadcházející akce" />
      <EventMemberList selected={id as string} />
      <div className="mt-6">{data?.event && <EventItem event={data.event} />}</div>
    </Item>
  );
}

EventItemPage.Layout = ({ children }: { children: React.ReactNode }) => {
  const { user } = useAuth();
  return <Layout showTopMenu={!user}>{children}</Layout>;
};
