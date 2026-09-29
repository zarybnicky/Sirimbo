import { EventWithAttendanceDocument } from '@/graphql/Event';
import { executeGraphql } from '@/lib/server/graphql';
import { stripHtml } from '@/lib/stripHtml';
import { getRequestContext } from '@/lib/server/tenant';
import type { Metadata } from 'next';
import { notFound } from 'next/navigation';
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
  const [{ id }, search, { tenant }] = await Promise.all([
    props.params,
    props.searchParams,
    getRequestContext(),
  ]);
  const token = Array.isArray(search.share) ? search.share[0] : search.share;
  const shareToken = /^[A-Za-z0-9_-]{32}$/.test(token ?? '') ? token : undefined;
  return {
    id,
    hasShareToken: !!shareToken,
    tenant,
    event: await loadEvent(id, shareToken ?? id),
  };
}

export async function generateMetadata(props: PageProps): Promise<Metadata> {
  const { id, hasShareToken, event, tenant } = await resolvePage(props);

  return {
    title: event?.name?.trim() || `Termín ${id}`,
    description: stripHtml(event?.summary) || undefined,
    alternates: { canonical: new URL(`/termin/${id}`, tenant.config.origin).toString() },
    robots: hasShareToken || !event ? { index: false, follow: false } : undefined,
  };
}

export default async function EventPage(props: PageProps) {
  const { id, hasShareToken, event } = await resolvePage(props);
  if (!event) notFound();
  return <EventPageClient id={id} initialEvent={event} hasShareToken={hasShareToken} />;
}
