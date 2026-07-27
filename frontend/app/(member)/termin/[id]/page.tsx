/* eslint-disable import-x/no-unused-modules */
import { runQuery } from '@/lib/server/postgresql';
import { stripHtml } from '@/lib/stripHtml';
import { getRequestTenant } from '@/tenant/server';
import type { Metadata } from 'next';
import { cache } from 'react';
import { EventPageClient } from './EventPageClient';
import { sharedEventInstance } from './termin.queries';

type PageProps = {
  params: Promise<{ id: string }>;
  searchParams: Promise<{ share?: string | string[] }>;
};

const shareTokenPattern = /^[A-Za-z0-9_-]{32}$/;
const eventIdPattern = /^\d{1,18}$/;

const loadSharedEvent = cache(
  async (
    id: string,
    token: string | undefined,
    tenantId: number,
  ) => {
    if (!eventIdPattern.test(id)) return null;
    const shareToken = token && shareTokenPattern.test(token) ? token : null;
    return runQuery(sharedEventInstance, { id, shareToken, tenantId }).then(x =>
      x.at(0),
    );
  },
);

async function resolvePage(props: PageProps) {
  const [{ id }, search, tenant] = await Promise.all([
    props.params,
    props.searchParams,
    getRequestTenant(),
  ]);
  const token = Array.isArray(search.share) ? search.share[0] : search.share;
  return {
    id,
    requestedToken: token,
    tenant,
    shared: await loadSharedEvent(id, token, tenant.id),
  };
}

export async function generateMetadata(props: PageProps): Promise<Metadata> {
  const { id, requestedToken, shared, tenant } = await resolvePage(props);
  const title = shared?.name?.trim() || `Termín ${id}`;
  const canonical = new URL(`/termin/${id}`, tenant.config.origin).toString();

  return {
    title,
    description: stripHtml(shared?.summary) || undefined,
    alternates: { canonical },
    robots: requestedToken || !shared ? { index: false, follow: false } : undefined,
  };
}

export default async function EventPage(props: PageProps) {
  const { id, shared } = await resolvePage(props);
  return <EventPageClient id={id} shared={shared} />;
}
