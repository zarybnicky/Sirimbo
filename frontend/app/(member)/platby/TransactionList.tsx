import React from 'react';
import {
  parseAsArrayOf,
  parseAsIsoDateTime,
  parseAsString,
  parseAsStringEnum,
  useQueryStates,
} from 'nuqs';
import { useQuery } from 'urql';
import type { AccountFragment } from '@/graphql/Payment';
import {
  TransactionAccountsDocument,
  TransactionFacetsDocument,
  TransactionsDocument,
} from '@/graphql/Payment';
import type { TransactionSource } from '@/graphql';
import {
  describePosting,
  moneyFormatter,
  numericDateWithYearFormatter,
} from '@/ui/format';
import Link from 'next/link';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { ListHeader } from '@/ui/ListHeader';
import { Table } from '@/ui/Table';
import { FacetFilter } from '@/ui/FacetFilter';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { DefinitionList } from '@/ui/DefinitionList';
import { compareText } from '@/lib/compare';
import {
  getPeriodRange,
  periodOptions,
  PeriodPicker,
  type PeriodUnit,
} from '@/ui/PeriodPicker';

const TRANSACTIONS_PAGE_SIZE = 50;
const sourceOptions: { value: TransactionSource; label: string }[] = [
  { value: 'AUTO_CREDIT', label: 'Úhrady z kreditu' },
  { value: 'MANUAL_CREDIT', label: 'Ruční změny kreditu' },
];
const defaultSources = sourceOptions.map((option) => option.value);

function accountLabel(account: AccountFragment | null | undefined) {
  if (!account) return '—';
  return `${account.personId === null ? 'Klub' : (account.person?.name ?? '—')} (${account.currency})`;
}

export function TransactionList() {
  const [filters, setFilters] = useQueryStates(
    {
      accountIds: parseAsArrayOf(parseAsString).withDefault([]),
      sources: parseAsArrayOf(
        parseAsStringEnum<TransactionSource>(defaultSources),
      ).withDefault(defaultSources),
      unit: parseAsStringEnum<PeriodUnit>(
        periodOptions.map((option) => option.value),
      ).withDefault('schoolyear'),
      date: parseAsIsoDateTime,
      search: parseAsString.withDefault(''),
    },
    { shallow: true, scroll: false },
  );
  const search = React.useDeferredValue(filters.search.trim());
  const searchPending = search !== filters.search.trim();
  const activeFacetCount =
    Number(filters.accountIds.length > 0) +
    Number(filters.sources.length !== defaultSources.length) +
    Number(filters.unit !== 'schoolyear');
  const [accounts] = useQuery({ query: TransactionAccountsDocument });
  const accountIds = filters.accountIds.length > 0 ? filters.accountIds : null;
  const period = { unit: filters.unit, date: filters.date ?? new Date() };
  const range = getPeriodRange(period.unit, period.date);
  const condition = {
    accountIds,
    sources: filters.sources.length === defaultSources.length ? null : filters.sources,
    date: {
      since: range.since.toISOString(),
      until: range.until.toISOString(),
    },
    search,
  };
  const [facets] = useQuery({
    query: TransactionFacetsDocument,
    variables: { ...condition, ...condition.date },
    pause: searchPending,
  });
  const counts: Record<string, Record<string, number>> = {};
  for (const { facet, value, count } of facets.data?.transactionFacetsList ?? []) {
    if (facet && value) (counts[facet] ??= {})[value] = count ?? 0;
  }
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: TransactionsDocument,
    variables: (offset) => ({
      first: TRANSACTIONS_PAGE_SIZE,
      offset,
      accountIds,
      condition,
    }),
    getTotal: (data) => data.transactions?.totalCount,
    limit: TRANSACTIONS_PAGE_SIZE,
    pageKey: 'page',
    pause: searchPending,
  });
  const change = (value: Partial<typeof filters>) => {
    void setFilters(value);
    void pagination.onPageChange(1);
  };
  const views: ({ value: string; label: string } & Pick<
    typeof filters,
    'accountIds' | 'sources'
  >)[] = [
    { value: 'all', label: 'Vše', accountIds: [], sources: defaultSources },
    {
      value: 'turnover',
      label: 'Obrat klubu',
      sources: defaultSources,
      accountIds: (accounts.data?.accountsList ?? [])
        .filter((x) => x.personId === null)
        .map((x) => x.id),
    },
    {
      value: 'deposits',
      label: 'Dobití kreditu',
      accountIds: [],
      sources: ['MANUAL_CREDIT'],
    },
  ];

  return (
    <>
      <ListHeader
        pagination={pagination}
        search={filters.search}
        onSearchChange={(search) => change({ search })}
        searchPlaceholder="Popis, VS nebo SS…"
        activeFilterCount={activeFacetCount}
        onReset={
          activeFacetCount > 0 || filters.search
            ? () =>
                change({
                  accountIds: [],
                  sources: defaultSources,
                  unit: 'schoolyear',
                  date: null,
                  search: '',
                })
            : undefined
        }
      >
        <FacetRadioGroup
          inline
          name="transaction-view"
          label="Pohled"
          value={
            views.find(
              (view) =>
                view.accountIds.length === filters.accountIds.length &&
                view.accountIds.every((id) => filters.accountIds.includes(id)) &&
                view.sources.length === filters.sources.length &&
                view.sources.every((x) => filters.sources.includes(x)),
            )?.value ?? ''
          }
          options={views.filter(
            (view) => view.value !== 'turnover' || view.accountIds.length > 0,
          )}
          onChange={(value) => {
            const view = views.find((x) => x.value === value)!;
            change({ accountIds: view.accountIds, sources: view.sources });
          }}
        />
        <FacetFilter
          inline
          label="Zdroj"
          value={filters.sources}
          defaultValue={defaultSources}
          options={sourceOptions.map((option) => ({
            ...option,
            count: facets.data ? (counts.source?.[option.value] ?? 0) : undefined,
          }))}
          onChange={(sources) => change({ sources })}
        />
        <FacetFilter
          inline
          label="Účty"
          value={filters.accountIds}
          maxVisibleOptions={5}
          moreLabel="Další"
          options={(accounts.data?.accountsList ?? [])
            .filter((x) => !!counts.account?.[x.id] || filters.accountIds.includes(x.id))
            .toSorted(
              (a, b) =>
                Number(a.personId !== null) - Number(b.personId !== null) ||
                compareText(accountLabel(a), accountLabel(b)),
            )
            .map((x) => ({
              value: x.id,
              label: accountLabel(x),
              count: facets.data ? (counts.account?.[x.id] ?? 0) : undefined,
            }))}
          onChange={(accountIds) => change({ accountIds })}
        />
        <PeriodPicker
          name="transactions-period"
          value={period}
          onChange={({ unit, date }) => change({ unit, date })}
        />
      </ListHeader>

      <Table
        columns={[
          'Datum',
          'Popis',
          'Účet',
          <span key="amount" className="block text-right">
            Částka
          </span>,
        ]}
        caption={
          filters.accountIds.length > 0 && (
            <DefinitionList
              className="text-sm tabular"
              items={(accounts.data?.accountsList || []).flatMap((x) =>
                filters.accountIds.includes(x.id)
                  ? ([[accountLabel(x), moneyFormatter.format(x)]] as const)
                  : [],
              )}
            />
          )
        }
        busy={fetching || searchPending}
        error={error ?? accounts.error ?? facets.error}
      >
        {(data?.transactions?.nodes ?? []).flatMap((transaction) =>
          transaction.postingsList.map(({ id, amount, account }, i) => (
            <tr key={id}>
              {i === 0 && (
                <>
                  <td
                    className="whitespace-nowrap text-neutral-11"
                    rowSpan={transaction.postingsList.length}
                  >
                    {numericDateWithYearFormatter.format(
                      new Date(transaction.effectiveDate),
                    )}
                  </td>
                  <td rowSpan={transaction.postingsList.length}>
                    {transaction.payment ? (
                      <Link
                        href={`/platby/${transaction.payment.id}`}
                        className="text-accent-11 underline"
                      >
                        {transaction.description ||
                          describePosting(transaction.payment, 'neutral')}
                      </Link>
                    ) : (
                      transaction.description ||
                      describePosting(transaction.payment, 'neutral')
                    )}
                    {(transaction.payment?.variableSymbol ||
                      transaction.payment?.specificSymbol) && (
                      <div className="inline-flex gap-1 pl-1 text-xs text-neutral-11">
                        {transaction.payment.variableSymbol && (
                          <div>VS: {transaction.payment.variableSymbol}</div>
                        )}
                        {transaction.payment.specificSymbol && (
                          <div>SS: {transaction.payment.specificSymbol}</div>
                        )}
                      </div>
                    )}
                  </td>
                </>
              )}
              <th scope="row" className="pl-2!">
                {accountLabel(account)}
              </th>
              <td className="whitespace-nowrap text-right">
                {account
                  ? moneyFormatter.format({ amount, currency: account.currency })
                  : amount}
              </td>
            </tr>
          )),
        )}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
