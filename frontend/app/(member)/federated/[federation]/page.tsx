import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { getRequestContext } from '@/lib/server/tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { FederationDocument } from '@/graphql/Federation';
import { Browser } from './Browser';

export const metadata = {
  title: 'Federation data',
  robots: { index: false, follow: false },
};

export default async function FederationPage({
  params,
}: {
  params: Promise<{ federation: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isLoggedIn) return <Layout requireUser />;
  const { federation } = await params;
  const { federation: item } = await executeGraphql(FederationDocument, {
    federation,
  });
  if (!item) notFound();

  return (
    <Layout requireUser>
      <PageHeader
        title={item.name}
        breadcrumbs={[{ label: 'Federations', href: '/federated' }, { label: item.name }]}
      />
      <Browser federation={federation} />
    </Layout>
  );
}
