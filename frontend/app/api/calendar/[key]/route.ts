import { groupCompetitionEvents, type CompetitionOfficialAssignment } from '@/calendar/competitions';
import { CalendarFeedDocument, type CalendarFeedQuery, type CalendarFeedVenueFragment } from '@/graphql/CalendarFeed';
import { normalizeClaims } from '@/lib/auth-claims';
import { executeGraphql } from '@/lib/server/graphql';
import { runQuery } from '@/lib/server/postgresql';
import { slugify } from '@/lib/slugify';
import { getTenant } from '@/tenant/catalog';
import { formatCategoryName } from '@/ui/csts';
import { formatAddress, formatEventName, formatOfficialRole, formatParticipant, listFormatter } from '@/ui/format';
import { compile } from 'html-to-text';
import ical, { ICalEventStatus } from 'ical-generator';
import jwt from 'jsonwebtoken';
import { calendarFeedOwner } from './calendar.queries';

const summaryToText = compile({ wordwrap: false });

type Competition = NonNullable<CalendarFeedQuery['competitions']>['nodes'][number];

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
    const pages: CalendarFeedQuery[] = [];
    let hasNextPage = false;
    do {
      const data = await executeGraphql(CalendarFeedDocument, {
        savedSearch: search.id,
        withCompetitions: search.with_competitions ?? false,
        withEvents: pages.length === 0,
        offset: pages.length * 300,
      }, {
        authorization: `Bearer ${token}`,
        'x-tenant-id': tenantId,
      });
      pages.push(data);
      hasNextPage = [data.competitions, data.officialEvents]
        .some((connection) => connection?.pageInfo.hasNextPage);
    } while (hasNextPage);
    const origin = getTenant(tenantId)?.config.origin ?? new URL(request.url).origin;
    return { origin, pages };
  }));

  const name = search.cross_tenant ? search.name : `${search.name}, ${search.tenant_name}`;
  const calendar = ical({ name, prodId: { company: 'Sirimbo', product: 'Calendar', language: 'CS' } });
  const competitions = new Map<string, Competition>();
  const officials: CompetitionOfficialAssignment[] = [];
  const venues = new Map<string, { event: CalendarFeedVenueFragment; origin: string }>();

  for (const { origin, pages } of calendars) {
    for (const data of pages) {
      for (const event of data.eventsList ?? []) {
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
          created: new Date(event.createdAt),
          lastModified: new Date(event.updatedAt),
          summary: formatEventName(event, viewer),
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

      for (const competition of data.competitions?.nodes ?? []) {
        const previous = competitions.get(competition.id);
        competitions.set(competition.id, {
          ...competition,
          entries: [...previous?.entries ?? [], ...competition.entries],
          results: [...previous?.results ?? [], ...competition.results],
        });
        if (competition.event && !venues.has(competition.event.id)) {
          venues.set(competition.event.id, { event: competition.event, origin });
        }
        if (competition.event) officials.push(...competition.officials.nodes.map((official) => ({
          ...official, event: competition.event!, federation: competition.federation, date: competition.startDate,
        })));
      }
      for (const event of data.officialEvents?.nodes ?? []) {
        if (!venues.has(event.id)) venues.set(event.id, { event, origin });
        officials.push(...event.officials.nodes.map((official) => ({
          ...official, event, federation: event.federation, date: event.startDate,
        })));
      }
    }
  }

  for (const group of groupCompetitionEvents([...competitions.values()], officials)) {
    const source = group.entries[0]?.competition.event ?? group.officials[0]?.event;
    const venue = source && venues.get(source.id);
    if (!venue || !group.date) continue;
    const { event, origin } = venue;
    const start = new Date(`${group.date}T00:00:00Z`);
    const end = new Date(start);
    end.setUTCDate(end.getUTCDate() + 1);

    const address = [event.streetAddress, event.postalCode, event.city, event.country].filter(Boolean).join(', ');
    const url = `${origin}/federated/${event.federation}/${event.externalId}`;

    calendar.createEvent({
      id: `competition-${event.federation}-${event.externalId}-${group.date}@sirimbo`,
      start, end, allDay: true,
      summary: event.name ?? 'Soutěž',
      description: [
        ...group.entries.map((entry) => [
          formatCategoryName(entry.competition),
          entry.competitor?.name,
          entry.cancelled && 'Odhlášeno',
        ].filter(Boolean).join(' — ')),
        ...group.officials.map((x) => `${formatOfficialRole(x.role)}: ${x.person?.canonicalName ?? ''}`),
        `Zobrazit v aplikaci: ${url}`,
      ].filter(Boolean).join('\n\n'),
      location: event.location || address ? {
        title: event.location || address,
        address: address || undefined,
        geo: event.venueLat && event.venueLng
          ? { lat: event.venueLat, lon: event.venueLng } : undefined,
      } : undefined,
      url,
      status: group.entries.length > 0 && group.entries.every((x) => x.cancelled) && group.officials.length === 0
        ? ICalEventStatus.CANCELLED : ICalEventStatus.CONFIRMED,
    });
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
