import * as React from 'react';
import { useAuth } from 'lib/data/use-auth';
import { EventMemberList } from 'components/EventMemberList';
import classNames from 'classnames';
import { Heading } from 'components/Heading';
import type { NextPageWithLayout } from 'pages/_app';
import { TitleBar } from 'components/layout/TitleBar';

const Page: NextPageWithLayout = () => {
  const { user } = useAuth();
  return (
    <>
      {!user && <Heading>Nadcházející akce</Heading>}
      <div className={classNames(user ? 'col-full-width p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8')}>
        {user && <TitleBar title="Nadcházející akce" />}
        <EventMemberList />
      </div>
    </>
  );
}

Page.staticTitle = "Nadcházející akce";
Page.hideTopMenuIfLoggedIn = true;

export default Page;
