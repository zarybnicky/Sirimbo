import type { CalendarDataQuery } from '@/graphql/Event';
import type {
  CompetitionParticipationFragment,
  CompetitionFragment,
} from '@/graphql/CompetitionParticipation';
import type { OfficialRole } from '@/graphql';
import type { OfficialPersonFragment } from '@/graphql/Federation';
import { compareText } from '@/lib/compare';

type Competition = CompetitionFragment & { participations: readonly CompetitionParticipationFragment[] };
export type CompetitionEntry = CompetitionParticipationFragment & { competition: CompetitionFragment };

export type CompetitionOfficialAssignment = {
  role: OfficialRole;
  person: OfficialPersonFragment | null;
};

export function groupCompetitionEvents(
  competitions: readonly Competition[] = [],
) {
  const entries = competitions.flatMap((competition) =>
    competition.participations.map((participation) => ({ ...participation, competition })),
  );
  return [...Map.groupBy(entries, (x) => x.competition.startDate)].flatMap(([date, day]) =>
    [...Map.groupBy(day, (x) => x.competition.event?.id)].map(([eventId, entries]) => ({
      key: `${date}:${eventId}`,
      date,
      name: entries[0]?.competition.event?.name ?? '',
      entries,
    })).toSorted((a, b) => compareText(a.name, b.name)),
  );
}

// Both the calendar and ICS use the dates of participations/assignments, which
// can differ from the federation event's start date.
export function federationCalendarDays(
  events: NonNullable<CalendarDataQuery['federationEvents']>['nodes'] = [],
) {
  return events.flatMap((event) => {
    const selected = Map.groupBy(event.selected, (p) => p.date);
    const club = Map.groupBy(event.club ?? [], (p) => p.date);
    const officials = Map.groupBy(event.officials, (o) => o.date);
    return [...new Set([...selected.keys(), ...officials.keys()])].toSorted().map((date) => ({
      event,
      date,
      selected: selected.get(date) ?? [],
      participations: [...new Map([
        ...club.get(date) ?? [], ...selected.get(date) ?? [],
      ].map((p) => [`${p.competitionId}:${p.competitorId}`, p])).values()],
      officials: officials.get(date) ?? [],
    }));
  });
}
