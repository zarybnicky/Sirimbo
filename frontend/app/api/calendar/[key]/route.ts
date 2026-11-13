import { federationCalendarDays } from '@/calendar/competitions';
import { savedSearchName } from '@/calendar/savedSearchName';
import { CalendarDataDocument, type CalendarDataQuery } from '@/graphql/Event';
import { normalizeClaims } from '@/lib/auth-claims';
import { executeGraphql } from '@/lib/server/graphql';
import { runQuery } from '@/lib/server/postgresql';
import { slugify } from '@/lib/slugify';
import { getTenant } from '@/tenant/catalog';
import { formatCategoryName } from '@/ui/csts';
import { formatAddress, formatEventName, formatOfficialRole, formatParticipant, listFormatter } from '@/ui/format';
import { formatDocument } from '@urql/core';
import { compile } from 'html-to-text';
import ical, { ICalEventStatus, ICalEventTransparency } from 'ical-generator';
import jwt from 'jsonwebtoken';
import { calendarFeedOwner } from './calendar.queries';

const summaryToText = compile({ wordwrap: false });

type CalendarDay = ReturnType<typeof federationCalendarDays>[number];

export async function GET(request: Request, { params }: { params: Promise<{ key: string }> }) {
  const { key } = await params;
  if (!/^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(key)) {
    return new Response('Calendar not found', { status: 404 });
  }
  const [search] = await runQuery(calendarFeedOwner, { key });
  if (!search) return new Response('Calendar not found', { status: 404 });

  const owner = search.claims as Record<string, unknown>;
  const token = jwt.sign(owner, process.env.JWT_SECRET!, { algorithm: 'HS256' });
  const claims = normalizeClaims(owner)!;

  const calendars = await Promise.all((search.tenant_ids ?? []).map(async (tenantId) => {
    const pages: CalendarDataQuery[] = [];
    let hasNextPage = false;
    do {
      const data = await executeGraphql(formatDocument(CalendarDataDocument), {
        savedSearch: search.id,
        withCompetitions: !!search.with_competitions && !search.can_register,
        withEvents: pages.length === 0,
        withCalendarExport: true,
        withRegistrationOptions: false,
        withFacets: false,
        withBirthdays: pages.length === 0 && !!search.with_birthdays && !search.can_register,
        registrationLimit: null,
        offset: pages.length * 300,
      }, {
        authorization: `Bearer ${token}`,
        'x-tenant-id': tenantId,
      });
      pages.push(data);
      hasNextPage = data.federationEvents?.pageInfo.hasNextPage ?? false;
    } while (hasNextPage);
    const origin = getTenant(tenantId)?.config.origin ?? new URL(request.url).origin;
    return { origin, pages };
  }));

  const name = `${search.tenant_name ?? 'Sirimbo'} – ${savedSearchName(search.name)}`;
  const calendar = ical({ name, prodId: { company: 'Sirimbo', product: 'Calendar', language: 'CS' } });
  let firstEvent = Infinity;
  let lastEvent = -Infinity;
  const days = new Map<string, CalendarDay & { origin: string }>();

  for (const { origin, pages } of calendars) {
    for (const data of pages) {
      for (const event of data.eventsList ?? []) {
        firstEvent = Math.min(firstEvent, Date.parse(event.since));
        lastEvent = Math.max(lastEvent, Date.parse(event.until));
        const { location, registrations, eventTrainersList, targetCohortsList } = event;

        const viewer = eventTrainersList.some((x) => claims.my_person_ids.includes(x.personId))
          ? 'trainer' : event.hasMyRegistration ? 'participant' : 'neutral';
        const trainers = listFormatter.format(eventTrainersList.flatMap((x) => x.person?.name ?? []));
        const participants = listFormatter.format(registrations.nodes.map(formatParticipant).filter(Boolean));
        const cohorts = listFormatter.format(targetCohortsList.flatMap((x) => x.cohort?.name ?? []));
        const place = location?.longName || location?.name || event.locationText || '';
        const url = `${origin}/termin/${event.id}`;

        calendar.createEvent({
          id: `event-${event.id}@sirimbo`,
          start: new Date(event.since),
          end: new Date(event.until),
          created: new Date(event.createdAt!),
          lastModified: new Date(event.updatedAt!),
          summary: formatEventName(event, viewer, viewer === 'neutral'),
          description: [
            trainers && `Trenéři: ${trainers}`,
            cohorts && `Tréninkové skupiny: ${cohorts}`,
            participants && `Účastníci: ${participants}`,
            event.parent && `Součást: ${event.parent.name || 'Soustředění'}\n${origin}/termin/${event.parent.id}`,
            summaryToText(event.summary ?? ''),
            `Zobrazit v aplikaci: ${url}`,
          ].filter(Boolean).join('\n\n'),
          location: place ? {
            title: place,
            address: location ? formatAddress(location?.address) || undefined : undefined,
            geo: location?.latitude && location.longitude
              ? { lat: location.latitude, lon: location.longitude } : undefined,
          } : undefined,
          url,
          status: event.isCancelled ? ICalEventStatus.CANCELLED : ICalEventStatus.CONFIRMED,
        });
      }

      for (const day of federationCalendarDays(data.federationEvents?.nodes)) {
        const key = `${day.event.id}:${day.date}`;
        const previous = days.get(key);
        days.set(key, {
          ...day,
          origin: previous?.origin ?? origin,
          participations: [...new Map([
            ...previous?.participations ?? [], ...day.participations,
          ].map((p) => [`${p.competitionId}:${p.competitorId}`, p])).values()],
          officials: [...new Map([
            ...previous?.officials ?? [], ...day.officials,
          ].map((o) => [`${o.role}:${o.person.id}`, o])).values()],
        });
      }
    }
  }

  for (const { date, event, origin, participations, officials } of days.values()) {
    const start = new Date(`${date}T00:00:00Z`);
    const end = new Date(start);
    end.setUTCDate(end.getUTCDate() + 1);
    firstEvent = Math.min(firstEvent, start.getTime());
    lastEvent = Math.max(lastEvent, end.getTime());

    const address = [event.streetAddress, event.postalCode, event.city, event.country].filter(Boolean).join(', ');
    const url = `${origin}/federated/${event.federation}/${event.externalId}`;

    calendar.createEvent({
      id: `competition-${event.federation}-${event.externalId}-${date}@sirimbo`,
      start, end, allDay: true,
      summary: event.name ?? 'Soutěž',
      description: [
        ...participations.map((entry) => [
          formatCategoryName(entry.competition),
          entry.competitor?.name,
          entry.cancelled && 'Odhlášeno',
        ].filter(Boolean).join(' — ')),
        ...officials.map((x) => `${formatOfficialRole(x.role)}: ${x.person?.canonicalName ?? ''}`),
        `Zobrazit v aplikaci: ${url}`,
      ].filter(Boolean).join('\n\n'),
      location: event.location || address ? {
        title: event.location || address,
        address: address || undefined,
        geo: event.venueLat && event.venueLng
          ? { lat: event.venueLat, lon: event.venueLng } : undefined,
      } : undefined,
      url,
      status: participations.length > 0 && participations.every((x) => x.cancelled) && officials.length === 0
        ? ICalEventStatus.CANCELLED : ICalEventStatus.CONFIRMED,
    });
  }

  if (Number.isFinite(firstEvent)) {
    const birthdays = new Set<string>();
    const firstDate = new Date(firstEvent);
    firstDate.setUTCHours(0, 0, 0, 0);
    const lastYear = new Date(lastEvent).getUTCFullYear();
    for (const { origin, pages } of calendars) {
      for (const person of pages[0]?.birthdays?.nodes ?? []) {
        if (!person.anniversary || birthdays.has(person.id)) continue;
        birthdays.add(person.id);
        // The resolver supplies month/day in a leap year, never the birth year.
        const reference = new Date(`${person.anniversary}T00:00:00Z`);
        const month = reference.getUTCMonth();
        for (let year = firstDate.getUTCFullYear(); year <= lastYear; year++) {
          const lastDay = new Date(Date.UTC(year, month + 1, 0)).getUTCDate();
          const start = new Date(Date.UTC(year, month, Math.min(reference.getUTCDate(), lastDay)));
          if (start < firstDate || start.getTime() >= lastEvent) continue;
          const url = `${origin}/clenove/${person.id}`;
          calendar.createEvent({
            id: `birthday-${person.id}-${year}@sirimbo`,
            start,
            allDay: true,
            summary: `Narozeniny: ${person.name}`,
            description: `Zobrazit v aplikaci: ${url}`,
            url,
            transparency: ICalEventTransparency.TRANSPARENT,
          });
        }
      }
    }
  }

  const filename = encodeURIComponent(`${name}.ics`).replaceAll(/[!'()*]/g, (x) => `%${x.codePointAt(0)!.toString(16)}`);
  return new Response(calendar.toString(), {
    headers: {
      'Content-Type': 'text/calendar; charset=utf-8',
      'Content-Disposition': `attachment; filename="${slugify(name)}.ics"; filename*=UTF-8''${filename}`,
      'Cache-Control': 'private, no-store',
      'X-Content-Type-Options': 'nosniff',
    },
  });
}
