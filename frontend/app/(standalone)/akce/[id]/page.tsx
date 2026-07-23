/* eslint-disable import-x/no-unused-modules */
import { runQuery } from '@/lib/server/postgresql';
import { notFound, redirect } from 'next/navigation';
import { eventSeriesEvents } from './akce.queries';

type LegacyEventRedirectPageProps = {
  params: Promise<{ id: string }>;
};

export default async function LegacyEventRedirectPage({
  params,
}: LegacyEventRedirectPageProps) {
  const { id } = await params;
  if (!/^\d+$/.test(id)) notFound();

  const events = await runQuery(eventSeriesEvents, { id });
  if (events.length === 0) notFound();

  redirect(
    events.length === 1
      ? `/termin/${events[0]?.id}`
      : `/terminy/${id}`,
  );
}
