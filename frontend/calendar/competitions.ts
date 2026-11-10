import type {
  CompetitionEntryFragment,
  CompetitionFragment,
  CompetitionResultFragment,
  CompetitionsQuery,
} from '@/graphql/CompetitionParticipation';
import type { OfficialRole } from '@/graphql';
import type { OfficialPersonFragment } from '@/graphql/Federation';
import { compareText } from '@/lib/compare';

type Competition = NonNullable<CompetitionsQuery['competitions']>['nodes'][number];
export type CompetitionEntry = CompetitionEntryFragment &
  Partial<CompetitionResultFragment> & { competition: Competition };

export type CompetitionOfficialAssignment = {
  date: string;
  federation: string;
  event: NonNullable<CompetitionFragment['event']>;
  role: OfficialRole;
  person: OfficialPersonFragment | null;
};

export function groupCompetitionEvents(
  competitions: readonly Competition[] = [],
  officials: readonly CompetitionOfficialAssignment[] = [],
) {
  const entries = competitions.flatMap((comp) => {
    const entries = new Map<string, CompetitionEntry>();
    for (const entry of [...comp.entries, ...comp.results]) {
      entries.set(entry.competitorId, {
        cancelled: false,
        ...entries.get(entry.competitorId),
        ...entry,
        competition: comp,
      });
    }
    return [...entries.values()];
  });
  return [
    ...Map.groupBy([...entries, ...officials], (x) =>
      'role' in x ? x.date : x.competition.startDate,
    ),
  ].flatMap(([date, day]) =>
    [...Map.groupBy(day, (x) => ('role' in x ? x.event.id : x.competition.event?.id))]
      .map(([eventId, items]) => {
        const entries = items.filter((x): x is CompetitionEntry => !('role' in x));
        const officials = [
          ...new Map(
            items
              .filter((x) => 'role' in x)
              .map((x) => [`${x.role}:${x.person?.id}`, x] as const),
          ).values(),
        ];
        const event = entries[0]?.competition.event ?? officials[0]?.event;
        return {
          key: `${date ?? ''}:${eventId ?? ''}`,
          date,
          name: event?.name ?? '',
          entries,
          officials,
        };
      })
      .toSorted((a, b) => compareText(a.name, b.name)),
  );
}

