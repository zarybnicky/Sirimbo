import * as React from 'react';
import { Item } from 'components/layout/Item';
import { useAuth } from 'lib/data/use-auth';
import { EventMemberList } from 'components/EventMemberList';
import classNames from 'classnames';
import { Heading } from 'components/Heading';
import type { NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const { user } = useAuth();
  return (
    <>
      {!user && <Heading>Nadcházející akce</Heading>}
      <div className={classNames(user ? 'col-full-width p-4 lg:py-8' : 'col-feature')}>
        {user && <Item.Titlebar title="Nadcházející akce" />}
        <EventMemberList />
      </div>
    </>
  );
}

Page.staticTitle = "Nadcházející akce";
Page.hideTopMenuIfLoggedIn = true;

export default Page;
