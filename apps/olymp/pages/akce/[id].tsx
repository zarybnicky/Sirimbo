import * as React from 'react';
import { EventDocument } from '@app/graphql/Event';
import { EventItem } from '@app/ui/EventItem';
import { useAuth } from '@app/ui/use-auth';
import { EventMemberList } from '@app/ui/EventMemberList';
import { useRouter } from 'next/router';
import { fromSlugArray } from '@app/ui/slugify';
import { NextSeo } from 'next-seo';
import { useQuery } from 'urql';
import { Layout } from 'components/layout/Layout';

const Page = () => {
  const router = useRouter();
  const { user } = useAuth();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({ query: EventDocument, variables: { id }, pause: !id });

  return (
    <Layout hideTopMenuIfLoggedIn>
      <NextSeo title={data?.event?.name || 'Nadcházející akce'} />
      <div className={user ? 'col-full-width p-4 lg:pb-8' : 'col-feature min-h-[60vh] mb-8'}>
        <EventItem id={id} />
        <EventMemberList selected={id} />
      </div>
    </Layout>
  );
};

export default Page;
