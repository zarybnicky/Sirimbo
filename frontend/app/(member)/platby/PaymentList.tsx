'use client';

import React from 'react';
import Link from 'next/link';
import { ChevronDown, SlidersHorizontal } from 'lucide-react';
import { cn } from '@/lib/cn';
import { buttonCls } from '@/ui/style';
import { parseAsArrayOf, parseAsString, parseAsStringEnum, useQueryStates } from 'nuqs';
import { useQuery } from 'urql';
import type { PaymentKind, PaymentStatus } from '@/graphql';
import { PaymentListDocument, PaymentListOptionsDocument } from '@/graphql/Payment';
import { useActionMap } from '@/lib/actions';
import { paymentActions } from '@/lib/actions/payment';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { ActionGroup } from '@/ui/ActionGroup';
import { FacetFilter } from '@/ui/FacetFilter';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { TextField } from '@/ui/fields/text';
import {
  describePosting,
  moneyFormatter,
  numericDateWithYearFormatter,
  paymentStatusLabels,
} from '@/ui/format';
import { Pagination } from '@/ui/Pagination';
import { Table } from '@/ui/Table';

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
  const [showFilters, setShowFilters] = React.useState(true);
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
  const actionMap = useActionMap(paymentActions, payments);
  const { total, page, limit } = pagination;
  const change = (value: Partial<typeof filters>) => {
    void setFilters(value);
    void pagination.onPageChange(1);
  };

  return (
    <>
      <div className="flex flex-wrap items-center gap-2 py-2">
        {!fetching && !error && (
          <p
            role="status"
            className="whitespace-nowrap text-xs tabular-nums text-neutral-11"
          >
            {total === 0
              ? '0 / 0'
              : `${(page - 1) * limit + 1}–${Math.min(page * limit, total)} / ${total}`}
          </p>
        )}
        <button
          type="button"
          aria-expanded={showFilters}
          aria-controls="payment-filters"
          className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
          onClick={() => setShowFilters((visible) => !visible)}
        >
          <SlidersHorizontal className="size-3" aria-hidden />
          Filtry{activeFacetCount > 0 && ` (${activeFacetCount})`}
          <ChevronDown
            className={cn('size-3', showFilters && 'rotate-180')}
            aria-hidden
          />
        </button>
        {(activeFacetCount > 0 || filters.search) && (
          <button
            type="button"
            className="px-2 py-1.5 text-xs text-accent-11 underline hover:text-accent-12"
            onClick={() => change({ status: '', kind: '', personIds: [], search: '' })}
          >
            Zrušit filtry
          </button>
        )}
        <TextField
          name="search"
          type="search"
          aria-label="Vyhledat platby"
          placeholder="Osoba, popis, VS nebo SS…"
          className="min-w-40 flex-1 sm:max-w-64 ml-auto"
          inputClassName="border-accent-6 bg-accent-2 py-1.5 text-sm text-neutral-12 placeholder:text-neutral-10 focus:border-accent-8 focus:ring-accent-8"
          value={filters.search}
          onChange={(event) => change({ search: event.currentTarget.value })}
        />
      </div>

      <div
        id="payment-filters"
        className={cn(
          'mb-3 w-full grid-cols-1 gap-x-3 gap-y-2 sm:grid-cols-[max-content_minmax(0,1fr)]',
          showFilters ? 'grid' : 'hidden',
        )}
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
      </div>

      <Table
        columns={[
          <span key="actions" className="sr-only">
            Akce
          </span>,
          'Platba',
          'Stav',
          'Splatnost',
          'Osoba',
          <span key="amount" className="block text-right">
            Částka
          </span>,
        ]}
        busy={fetching}
        error={error ?? options.error}
        empty="Žádné platby."
      >
        {payments.map((payment) => {
          const [debtor, ...others] = payment.paymentDebtorsList;
          const rowSpan = others.length + 1;
          return (
            <React.Fragment key={payment.id}>
              <tr className="border-t border-neutral-4 align-top">
                <td rowSpan={rowSpan} className="w-0">
                  <ActionGroup variant="row" actions={actionMap.get(payment.id)!} />
                </td>
                <th scope="row" rowSpan={rowSpan}>
                  <Link
                    href={`/platby/${payment.id}`}
                    className="text-accent-11 underline"
                  >
                    {describePosting(payment, 'neutral') || `Platba ${payment.id}`}
                  </Link>
                  {(payment.variableSymbol || payment.specificSymbol) && (
                    <div className="flex flex-wrap gap-x-2 text-xs text-neutral-11">
                      {payment.variableSymbol && (
                        <span>VS: {payment.variableSymbol}</span>
                      )}
                      {payment.specificSymbol && (
                        <span>SS: {payment.specificSymbol}</span>
                      )}
                    </div>
                  )}
                </th>
                <td rowSpan={rowSpan} className="text-neutral-11">
                  {paymentStatusLabels[payment.status]}
                </td>
                <td rowSpan={rowSpan} className="whitespace-nowrap text-neutral-11">
                  {payment.dueAt
                    ? numericDateWithYearFormatter.format(new Date(payment.dueAt))
                    : '—'}
                </td>
                <td className="pl-2!">{debtor?.person?.name ?? '—'}</td>
                <td className="whitespace-nowrap text-right">
                  {moneyFormatter.format(debtor?.price ?? null) || '—'}
                </td>
              </tr>
              {others.map((debtor) => (
                <tr key={debtor.id} className="border-t border-neutral-4 align-top">
                  <td>{debtor.person?.name ?? '—'}</td>
                  <td className="whitespace-nowrap text-right">
                    {moneyFormatter.format(debtor.price) || '—'}
                  </td>
                </tr>
              ))}
            </React.Fragment>
          );
        })}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
