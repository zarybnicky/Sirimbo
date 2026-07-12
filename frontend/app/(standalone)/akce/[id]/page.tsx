/* eslint-disable import-x/no-unused-modules */
import { EventSeriesDocument } from '@/graphql/Event';
import { executeGraphql } from '@/lib/server/graphql';
import { notFound, redirect } from 'next/navigation';

type LegacyEventRedirectPageProps = {
  params: Promise<{ id: string }>;
};

export default async function LegacyEventRedirectPage({
  params,
}: LegacyEventRedirectPageProps) {
  const { id } = await params;
  if (!/^\d+$/.test(id)) notFound();

  const { eventSeries: series } = await executeGraphql(
    EventSeriesDocument,
    { id },
  );
  if (!series) notFound();

  redirect(
    series.eventsList.length === 1
      ? `/termin/${series.eventsList[0]!.id}`
      : `/terminy/${series.id}`,
  );
}
