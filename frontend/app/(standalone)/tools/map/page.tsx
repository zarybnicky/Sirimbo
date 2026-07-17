/* eslint-disable import-x/no-unused-modules */
import type { Metadata } from 'next';
import { runQuery } from '@/lib/server/postgresql';
import { competitionEventLocations } from '../tools.queries';
import EventMap from './EventMap';

export const dynamic = 'force-dynamic';

export const metadata: Metadata = {
  title: 'Competition map',
  robots: { index: false },
};

export default async function Page() {
  const year = new Date().getFullYear();
  const events = await runQuery(competitionEventLocations, {
    federation: 'csts',
    fromDate: `${year - 3}-12-31`,
    toDate: `${year}-12-31`,
  });

  return <EventMap events={events} />;
}
