import * as React from 'react';
import { EventDocument } from 'lib/graphql/Event';
import { Item } from 'components/layout/Item';
import { EventItem } from 'components/EventItem';
import { useAuth } from 'lib/data/use-auth';
import { EventMemberList } from 'components/EventMemberList';
import { useRouter } from 'next/router';
import classNames from 'classnames';
import { Heading } from 'components/Heading';
import type { NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from 'lib/slugify';
import { NextSeo } from 'next-seo';
import { useGqlQuery } from 'lib/query';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const { user } = useAuth();
  const id = fromSlugArray(router.query.id);
  const { data } = useGqlQuery(EventDocument, { id }, { enabled: !!id });

  return (
    <>
      <NextSeo title={data?.event?.name || 'Nadcházející akce'} />
      {!user && <Heading>Nadcházející akce</Heading>}
      <div className={classNames(user ? 'col-full-width p-4 lg:py-8' : 'col-feature')}>
        {user && <Item.Titlebar title="Nadcházející akce" />}
        <EventMemberList selected={id} />
        <div className="mt-6">{data?.event && <EventItem event={data.event} />}</div>
      </div>
    </>
  );
};

Page.hideTopMenuIfLoggedIn = true;

export default Page;
