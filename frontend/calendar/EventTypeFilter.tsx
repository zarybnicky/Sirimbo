import { eventTypes, calendarFilterParsers } from '@/calendar/state';
import { FacetFilter } from '@/ui/FacetFilter';
import { formatEventType } from '@/ui/format';
import { useQueryState } from 'nuqs';

export function EventTypeFilter({ counts }: { counts?: Record<string, number> }) {
  const [selected, setSelected] = useQueryState('types', calendarFilterParsers.types);
  return (
    <FacetFilter
      label="Typy"
      value={selected}
      defaultValue={eventTypes}
      options={eventTypes.map((type) => ({
        value: type,
        label: formatEventType(type),
        count: counts ? (counts[type] ?? 0) : undefined,
      }))}
      onChange={setSelected}
    />
  );
}
