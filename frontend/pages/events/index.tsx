import * as React from 'react';
import { Item } from 'components/layout/Item';
import { useAuth } from 'lib/data/use-auth';
import { Layout } from 'components/layout/Layout';
import { EventMemberList } from 'components/EventMemberList';
import classNames from 'classnames';

export default function EventListPage() {
  const { user } = useAuth();
  return (
    <Item className={classNames(user ? 'col-full bg-stone-100' : 'col-feature mt-6')}>
      <Item.Titlebar title="Nadcházející akce" />
      <EventMemberList />
    </Item>
  );
}

EventListPage.Layout = function ThisLayout({ children }: { children: React.ReactNode }) {
  const { user } = useAuth();
  return <Layout showTopMenu={!user}>{children}</Layout>;
};
