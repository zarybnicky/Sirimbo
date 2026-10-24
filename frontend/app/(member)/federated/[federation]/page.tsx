import { notFound } from 'next/navigation';
import type { Metadata } from 'next';
import { cache } from 'react';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationListDocument } from '@/graphql/Federation';
import { Browser } from './Browser';

type Props = {
  params: Promise<{ federation: string }>;
};

const getFederation = cache(async (federation: string) => {
  const { federationsList } = await executeGraphql(FederationListDocument, {
    condition: { code: federation },
    withCounts: false,
  });
  const item = federationsList?.[0];
  if (!item) notFound();
  return item;
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return { robots: { index: false, follow: false } };
  const { federation } = await params;
  const item = await getFederation(federation);
  return { title: item.name, robots: { index: false, follow: false } };
}

export default async function FederationPage({ params }: Props) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation } = await params;
  const item = await getFederation(federation);

  return (
    <Layout requireUser>
      <PageHeader
        title={item.name}
        breadcrumbLabel={item.name}
        breadcrumbs={[{ label: 'Soutěžní data', href: '/federated' }]}
      />
      <Browser federation={federation} />
    </Layout>
  );
}
