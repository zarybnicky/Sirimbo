import { LocationPageDocument } from '@/graphql/Location';
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
  const result = await executeGraphql(LocationPageDocument, {
    id,
    start,
  });
  return result.location && { data: result, start };
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { id } = await params;
  const result = await getLocation(id);
  if (!result?.data?.location) notFound();

  return {
    title: result.data.location.longName || result.data.location.name,
    robots: { index: false, follow: false },
  };
}

export default async function LocationPage({ params }: Props) {
  const { id } = await params;
  const result = await getLocation(id);
  if (!result) notFound();

  return (
    <Layout>
      <Location initialData={result.data} start={result.start} />
    </Layout>
  );
}
