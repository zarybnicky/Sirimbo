import { TenantLocationPageDocument } from '@/graphql/Tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { cache } from 'react';
import { Location } from './Location';

type Props = {
  params: Promise<{ id: string }>;
};

const getLocation = cache(async (id: string) => {
  if (!/^\d{1,18}$/.test(id)) return null;
  const start = new Date().toISOString();
  const result = await executeGraphql(TenantLocationPageDocument, {
    id,
    start,
  });
  return result.tenantLocation && { data: result, start };
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { id } = await params;
  const result = await getLocation(id);
  if (!result) notFound();

  return {
    title: result.data.tenantLocation!.name,
    robots: { index: false, follow: false },
  };
}

export default async function LocationPage({ params }: Props) {
  const { id } = await params;
  const result = await getLocation(id);
  if (!result) notFound();

  return (
    <Layout requireMember>
      <Location initialData={result.data} start={result.start} />
    </Layout>
  );
}
