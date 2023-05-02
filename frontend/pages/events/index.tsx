import * as React from 'react';
import { Item } from 'components/layout/Item';
import { useAuth } from 'lib/data/use-auth';
import { Layout } from 'components/layout/Layout';
import { EventMemberList } from 'components/EventMemberList';

export default function EventListPage() {
  return (
    <Item className="col-feature mt-6">
      <Item.Titlebar title="Nadcházející akce" />
      <EventMemberList/>
    </Item>
  );
}

EventListPage.Layout = ({ children }: { children: React.ReactNode }) => {
  const { user } = useAuth();
  return <Layout showTopMenu={!user}>{children}</Layout>;
};
