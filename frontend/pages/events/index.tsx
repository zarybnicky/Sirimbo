import * as React from 'react';
import { Item } from 'components/layout/Item';
import { useAuth } from 'lib/data/use-auth';
import { Layout } from 'components/layout/Layout';
import { EventMemberList } from 'components/EventMemberList';
import classNames from 'classnames';
import { Heading } from 'components/Heading';

export default function EventListPage() {
  const { user } = useAuth();
  return (
    <>
      {!user && (
        <Heading
          image=""
          text="Nadcházející akce"
          color={{ r: 255, g: 60, b: 60, a: 90 }}
        />
      )}
      <Item className={classNames(user ? 'col-full bg-stone-100' : 'col-feature')}>
        {user && <Item.Titlebar title="Nadcházející akce" />}
        <EventMemberList />
      </Item>
    </>
  );
}

EventListPage.Layout = function ThisLayout({ children }: { children: React.ReactNode }) {
  const { user } = useAuth();
  return <Layout showTopMenu={!user}>{children}</Layout>;
};
