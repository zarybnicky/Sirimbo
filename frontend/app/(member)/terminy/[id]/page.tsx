/* eslint-disable import-x/no-unused-modules */
import { EventSeriesDocument } from '@/graphql/Event';
import { executeGraphql } from '@/lib/server/graphql';
import { Layout } from '@/ui/Layout';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
import { cache } from 'react';
import { EventSeries } from './EventSeries';

type Props = {
  params: Promise<{ id: string }>;
};

const getEventSeries = cache(async (id: string) => {
  if (!/^\d{1,18}$/.test(id)) return null;
  return executeGraphql(EventSeriesDocument, { id }).then((x) => x.eventSeries);
});

export async function generateMetadata({ params }: Props): Promise<Metadata> {
  const { id } = await params;
  const series = await getEventSeries(id);
  if (!series) notFound();

  return {
    title: series.name || 'Termíny',
    alternates: { canonical: `/terminy/${series.id}` },
  };
}

export default async function EventSeriesPage({ params }: Props) {
  const { id } = await params;
  const series = await getEventSeries(id);
  if (!series) notFound();

  return (
    <Layout hideTopMenuIfLoggedIn includeTenantSeo={false}>
      <EventSeries initialSeries={series} />
    </Layout>
  );
}
