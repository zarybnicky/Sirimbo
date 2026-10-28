import { LocationOptionsDocument } from '@/graphql/Location';
import { useAuth } from '@/lib/auth';
import { FacetFilter } from '@/ui/FacetFilter';
import { calendarFilterParsers } from '@/calendar/state';
import { useQueryState } from 'nuqs';
import { useQuery } from 'urql';

export function LocationFilter({ counts }: { counts?: Record<string, number> }) {
  const [ids, setIds] = useQueryState('location', calendarFilterParsers.location);
  const auth = useAuth();
  const [{ data }] = useQuery({ query: LocationOptionsDocument });
  const locations = (data?.tenant?.locationsList ?? []).filter(
    (x) => auth.isAdmin || x.showInLists || ids.includes(x.id),
  );

  return (
    <FacetFilter
      label="Místa"
      value={ids}
      onChange={setIds}
      options={locations.map((location) => ({
        value: location.id,
        label: location.name,
        count: counts ? (counts[location.id] ?? 0) : undefined,
      }))}
    />
  );
}
