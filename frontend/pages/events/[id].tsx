import * as React from 'react';
import { useEventQuery, useMyEventsQuery } from 'lib/graphql/Event';
import { Item } from 'components/layout/Item';
import { EventItem } from 'components/EventItem';
import { useAuth } from 'lib/data/use-auth';
import { EventMemberList } from 'components/EventMemberList';
import { useRouter } from 'next/router';
import classNames from 'classnames';
import { Heading } from 'components/Heading';
import { type NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const { user } = useAuth();
  const id = fromSlugArray(router.query.id);
  const { data } = useEventQuery({ id }, { enabled: !!id });

  return (
    <>
      {!user && <Heading>Nadcházející akce</Heading>}
      <Item className={classNames(user ? 'col-full bg-stone-100' : 'col-feature')}>
        {user && <Item.Titlebar title="Nadcházející akce" />}
        <EventMemberList selected={id} />
        <div className="mt-6">{data?.event && <EventItem event={data.event} />}</div>
      </Item>
    </>
  );
};

Page.hideTopMenuIfLoggedIn = true;

export default Page;
