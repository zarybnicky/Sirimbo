import { useQueryState } from 'nuqs';
import { calendarFilterParsers } from '@/calendar/state';
import { useQuery } from 'urql';
import { FacetFilter } from '@/ui/FacetFilter';
import { isTruthy } from '@/lib/truthyFilter';
import { compareText } from '@/lib/compare';
import { PersonOptionsDocument } from '@/graphql/Person';

export function ParticipantFilter({ counts }: { counts?: Record<string, number> }) {
  const [ids, setIds] = useQueryState('participants', calendarFilterParsers.participants);
  const [{ data }] = useQuery({
    query: PersonOptionsDocument,
    variables: { condition: { membership: { state: 'CURRENT' } } },
  });
  const available = (data?.people?.nodes || [])
    .filter(isTruthy)
    .filter((person) => !counts || (counts[person.id] ?? 0) > 0 || ids.includes(person.id))
    .toSorted((a, b) =>
      compareText(a.lastName, b.lastName) || compareText(a.firstName, b.firstName),
    );
  const selected = available.filter((x) => ids.includes(x.id));
  const other = available.filter((x) => !ids.includes(x.id));

  return (
    <FacetFilter
      label="Účastníci"
      value={ids}
      options={[...selected, ...other].map((person) => ({
        value: person.id,
        label: person.name,
        count: counts?.[person.id] || undefined,
      }))}
      onChange={setIds}
    />
  );
}
