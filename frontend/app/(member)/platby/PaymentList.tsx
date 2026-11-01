'use client';

import { parseAsArrayOf, parseAsString, parseAsStringEnum, useQueryStates } from 'nuqs';
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
  const [options] = useQuery({ query: PaymentListOptionsDocument });
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: PaymentListDocument,
    variables: (offset) => ({
      first: 50,
      offset,
      condition: {
        status: filters.status || undefined,
        kind: filters.kind || undefined,
        personIds: filters.personIds.length > 0 ? filters.personIds : null,
        search: filters.search.trim(),
      },
    }),
    getTotal: (data) => data.payments?.totalCount,
    limit: 50,
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
        busy={fetching}
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
          options={statusOptions}
          onChange={(status) => change({ status })}
        />
        <FacetRadioGroup
          inline
          name="kind"
          label="Typ"
          value={filters.kind}
          options={kindOptions}
          onChange={(kind) => change({ kind })}
        />
        <FacetFilter
          inline
          label="Osoby"
          value={filters.personIds}
          maxVisibleOptions={5}
          moreLabel="Další"
          options={(options.data?.people?.nodes ?? []).map((x) => ({
            value: x.id,
            label: x.name,
          }))}
          onChange={(personIds) => change({ personIds })}
        />
      </ListHeader>

      <PaymentTable payments={payments} busy={fetching} error={error ?? options.error} />
      <Pagination {...pagination} />
    </>
  );
}
