/* eslint-disable import-x/no-unused-modules */
import { EventWithAttendanceDocument } from '@/graphql/Event';
import { executeGraphql } from '@/lib/server/graphql';
import { stripHtml } from '@/lib/stripHtml';
import { getRequestTenant } from '@/lib/server/tenant';
import type { Metadata } from 'next';
import { cache } from 'react';
import { EventPageClient } from './EventPageClient';

type PageProps = {
  params: Promise<{ id: string }>;
  searchParams: Promise<{ share?: string | string[] }>;
};

const loadEvent = cache(async (id: string, share: string) => {
  if (!/^\d{1,18}$/.test(id)) return null;
  const data = await executeGraphql(
    EventWithAttendanceDocument,
    { id },
    { 'x-event-share': share },
  );
  return data.event;
});

async function resolvePage(props: PageProps) {
  const [{ id }, search, tenant] = await Promise.all([
    props.params,
    props.searchParams,
    getRequestTenant(),
  ]);
  const token = Array.isArray(search.share) ? search.share[0] : search.share;
  const shareToken = /^[A-Za-z0-9_-]{32}$/.test(token ?? '') ? token : undefined;
  return {
    id,
    requestedToken: token,
    hasShareToken: !!shareToken,
    tenant,
    event: await loadEvent(id, shareToken ?? id),
  };
}

export async function generateMetadata(props: PageProps): Promise<Metadata> {
  const { id, requestedToken, event, tenant } = await resolvePage(props);
  const title = event?.name?.trim() || `Termín ${id}`;
  const canonical = new URL(`/termin/${id}`, tenant.config.origin).toString();

  return {
    title,
    description: stripHtml(event?.summary) || undefined,
    alternates: { canonical },
    robots: requestedToken || !event ? { index: false, follow: false } : undefined,
  };
}

export default async function EventPage(props: PageProps) {
  const { id, hasShareToken, event } = await resolvePage(props);
  return (
    <EventPageClient id={id} initialEvent={event} hasShareToken={hasShareToken} />
  );
}
