'use client';

import { parseAsArrayOf, parseAsString, parseAsStringEnum, useQueryStates } from 'nuqs';
import React from 'react';
import { useQuery } from 'urql';
import type { PaymentKind, PaymentStatus } from '@/graphql';
import { PaymentListDocument, PaymentListOptionsDocument } from '@/graphql/Payment';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { FacetFilter } from '@/ui/FacetFilter';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { paymentStatusLabels } from '@/ui/format';
import { Pagination } from '@/ui/Pagination';
import { ListHeader } from '@/ui/ListHeader';
import { PaymentTable } from './PaymentTable';

const statusOptions: { value: PaymentStatus | ''; label: string }[] = [
  { value: '', label: 'Všechny' },
  { value: 'UNPAID', label: paymentStatusLabels.UNPAID },
  { value: 'PAID', label: paymentStatusLabels.PAID },
  { value: 'TENTATIVE', label: paymentStatusLabels.TENTATIVE },
];
const kindOptions: { value: PaymentKind | ''; label: string }[] = [
  { value: '', label: 'Všechny' },
  { value: 'COHORT_SUBSCRIPTION', label: 'Členské příspěvky' },
  { value: 'EVENT', label: 'Události' },
  { value: 'OTHER', label: 'Ostatní' },
];

export function PaymentList() {
  const [filters, setFilters] = useQueryStates(
    {
      status: parseAsStringEnum(statusOptions.map((x) => x.value)).withDefault(''),
      kind: parseAsStringEnum(kindOptions.map((x) => x.value)).withDefault(''),
      personIds: parseAsArrayOf(parseAsString).withDefault([]),
      search: parseAsString.withDefault(''),
    },
    { shallow: true, scroll: false },
  );
  const activeFacetCount =
    Number(filters.status !== '') +
    Number(filters.kind !== '') +
    Number(filters.personIds.length > 0);
  const search = React.useDeferredValue(filters.search.trim());
  const searchPending = search !== filters.search.trim();
  const condition = {
    status: filters.status || undefined,
    kind: filters.kind || undefined,
    personIds: filters.personIds.length > 0 ? filters.personIds : null,
    search,
  };
  const [options] = useQuery({
    query: PaymentListOptionsDocument,
    variables: condition,
    pause: searchPending,
  });
  const counts: Record<string, Record<string, number>> = {};
  for (const { facet, value, count } of options.data?.paymentFacetsList ?? []) {
    if (facet && value != null) (counts[facet] ??= {})[value] = count ?? 0;
  }
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: PaymentListDocument,
    variables: (offset) => ({
      first: 50,
      offset,
      condition,
    }),
    getTotal: (data) => data.payments?.totalCount,
    limit: 50,
    pause: searchPending,
  });
  const payments = data?.payments?.nodes ?? [];
  const change = (value: Partial<typeof filters>) => {
    void setFilters(value);
    void pagination.onPageChange(1);
  };

  return (
    <>
      <ListHeader
        pagination={!fetching && !error ? pagination : undefined}
        search={filters.search}
        onSearchChange={(search) => change({ search })}
        searchPlaceholder="Osoba, popis, VS nebo SS…"
        activeFilterCount={activeFacetCount}
        onReset={
          activeFacetCount > 0 || filters.search
            ? () => change({ status: '', kind: '', personIds: [], search: '' })
            : undefined
        }
      >
        <FacetRadioGroup
          inline
          name="status"
          label="Stav"
          value={filters.status}
          options={statusOptions.map((option) => ({
            ...option,
            count: options.data ? (counts.status?.[option.value] ?? 0) : undefined,
          }))}
          onChange={(status) => change({ status })}
        />
        <FacetRadioGroup
          inline
          name="kind"
          label="Typ"
          value={filters.kind}
          options={kindOptions.map((option) => ({
            ...option,
            count: options.data ? (counts.kind?.[option.value] ?? 0) : undefined,
          }))}
          onChange={(kind) => change({ kind })}
        />
        <FacetFilter
          inline
          label="Osoby"
          value={filters.personIds}
          maxVisibleOptions={5}
          moreLabel="Další"
          options={(options.data?.paymentFacetsList ?? [])
            .flatMap(({ facet, value, label, count }) =>
              facet === 'person' && value && label
                ? [{ value, label, count: count ?? 0 }]
                : [],
            )
            .toSorted((a, b) => a.label.localeCompare(b.label, 'cs'))}
          onChange={(personIds) => change({ personIds })}
        />
      </ListHeader>

      <PaymentTable
        payments={payments}
        busy={fetching || searchPending}
        error={error ?? options.error}
      />
      <Pagination {...pagination} />
    </>
  );
}
