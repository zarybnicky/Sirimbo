import * as React from 'react';
import { EventDocument } from '@app/graphql/Event';
import { EventItem } from '@app/ui/EventItem';
import { useAuth } from '@app/ui/use-auth';
import { EventMemberList } from '@app/ui/EventMemberList';
import { useRouter } from 'next/router';
import { Heading } from '@app/ui/Heading';
import type { NextPageWithLayout } from 'pages/_app';
import { fromSlugArray } from '@app/ui/slugify';
import { NextSeo } from 'next-seo';
import { useQuery } from 'urql';
import { TitleBar } from '@app/ui/TitleBar';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const { user } = useAuth();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({ query: EventDocument, variables:{ id }, pause: !id });

  return (
    <>
      <NextSeo title={data?.event?.name || 'Nadcházející akce'} />
      {!user && <Heading>Nadcházející akce</Heading>}
      <div className={user ? 'col-full-width p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'}>
        {user && <TitleBar title="Nadcházející akce" />}
        <EventMemberList selected={id} />
        <div className="mt-6"><EventItem id={id} /></div>
      </div>
    </>
  );
};

Page.hideTopMenuIfLoggedIn = true;

export default Page;
