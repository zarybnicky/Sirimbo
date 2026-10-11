'use client';

import { debounce, parseAsInteger, parseAsString, useQueryStates } from 'nuqs';
import { TextField } from '@/ui/fields/text';

const textFilter = parseAsString
  .withDefault('')
  .withOptions({ limitUrlUpdates: debounce(300) });

export function Filters() {
  const [filters, setFilters] = useQueryStates(
    { search: textFilter, date: textFilter, page: parseAsInteger },
    { shallow: false, scroll: false },
  );
  return (
    <div className="flex flex-wrap items-end gap-3">
      <TextField
        name="search"
        label="Name, location or event ID"
        value={filters.search}
        onChange={(event) => setFilters({ search: event.target.value, page: null })}
      />
      <TextField
        name="date"
        label="Date"
        type="date"
        value={filters.date}
        onChange={(event) => setFilters({ date: event.target.value, page: null })}
      />
    </div>
  );
}
