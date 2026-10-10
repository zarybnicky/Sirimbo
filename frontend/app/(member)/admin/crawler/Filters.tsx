'use client';

import { debounce, parseAsString, useQueryStates } from 'nuqs';
import { useRouter } from 'next/navigation';
import { TextField } from '@/ui/fields/text';
import { Checkbox } from '@/ui/fields/checkbox';
import { buttonCls } from '@/ui/style';

const textFilter = parseAsString
  .withDefault('')
  .withOptions({ limitUrlUpdates: debounce(300) });

export function Filters() {
  const router = useRouter();
  const [filters, setFilters] = useQueryStates(
    {
      federation: textFilter,
      kind: textFilter,
      all: parseAsString.withDefault(''),
    },
    { shallow: false, scroll: false },
  );

  return (
    <div className="flex flex-wrap items-end gap-3">
      <TextField
        name="federation"
        label="federation"
        className="w-32"
        value={filters.federation}
        onChange={(event) => setFilters({ federation: event.target.value })}
      />
      <TextField
        name="kind"
        label="kind"
        className="w-48"
        value={filters.kind}
        onChange={(event) => setFilters({ kind: event.target.value })}
      />
      <Checkbox
        name="all"
        label="Show completed"
        checked={filters.all === '1'}
        onChange={(event) => setFilters({ all: event.currentTarget.checked ? '1' : '' })}
      />
      <button
        type="button"
        className={buttonCls({ variant: 'outline' })}
        onClick={() => router.refresh()}
      >
        Refresh
      </button>
    </div>
  );
}
