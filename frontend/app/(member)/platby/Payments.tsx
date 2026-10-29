'use client';

import { PageHeader } from '@/ui/PageHeader';
import React from 'react';
import {
  parseAsArrayOf,
  parseAsIsoDateTime,
  parseAsString,
  parseAsStringEnum,
  useQueryState,
  useQueryStates,
} from 'nuqs';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { useClient, useQuery } from 'urql';
import { TransactionAccountsDocument, TransactionsDocument } from '@/graphql/Payment';
import type { TransactionSource } from '@/graphql';
import type { AccountFragment } from '@/graphql/Payment';
import { PersonAccountsDocument } from '@/graphql/Person';
import {
  describePosting,
  moneyFormatter,
  numericDateWithYearFormatter,
} from '@/ui/format';
import { buttonCls } from '@/ui/style';
import { exportBalanceSheet } from '@/ui/reports/export-balance-sheet';
import Link from 'next/link';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { Table } from '@/ui/Table';
import { FacetFilter } from '@/ui/FacetFilter';
import { FacetRadioGroup } from '@/ui/FacetRadioGroup';
import { DefinitionList } from '@/ui/DefinitionList';
import { TextField } from '@/ui/fields/text';
import { compareText } from '@/lib/compare';
import { cn } from '@/lib/cn';
import {
  getPeriodRange,
  periodOptions,
  PeriodPicker,
  type PeriodUnit,
} from '@/ui/PeriodPicker';
import { ChevronDown, SlidersHorizontal } from 'lucide-react';
import { PaymentList } from './PaymentList';

const TRANSACTIONS_PAGE_SIZE = 50;
const sourceOptions: { value: TransactionSource; label: string }[] = [
  { value: 'AUTO_CREDIT', label: 'Úhrady z kreditu' },
  { value: 'MANUAL_CREDIT', label: 'Ruční změny kreditu' },
];
const defaultSources = sourceOptions.map((option) => option.value);

export function Payments() {
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  return (
    <>
      <PageHeader title="Platby" />
      <TabMenu selected={tab === 'unpaid' ? 'payments' : tab} onSelect={setTab}>
        <Tab id="info" title="Stav kreditu">
          <AccountOverview />
        </Tab>
        <Tab id="payments" title="Platby">
          <PaymentList />
        </Tab>
        <Tab id="transactions" title="Transakce">
          <Transactions />
        </Tab>
      </TabMenu>
    </>
  );
}

const AccountOverview = React.memo(function AccountOverview() {
  const client = useClient();
  const [{ data, fetching, error }] = useQuery({ query: PersonAccountsDocument });

  return (
    <>
      <button
        type="button"
        className={buttonCls({ className: 'mb-3' })}
        onClick={() => exportBalanceSheet(client)}
      >
        Přehled plateb 2023
      </button>
      <Table
        columns={[
          'Osoba',
          <span key="balance" className="block text-right">
            Zůstatek
          </span>,
        ]}
        busy={fetching}
        error={error}
      >
        {(data?.people?.nodes ?? []).map((person) => (
          <tr key={person.id} className="border-t border-neutral-4 align-top">
            <th scope="row">{person.name}</th>
            <td className="whitespace-nowrap text-right">
              {person.accountsList.map((account) => (
                <div key={account.id}>
                  {moneyFormatter.format({
                    amount: account.balance ?? '0',
                    currency: account.currency,
                  })}
                </div>
              ))}
            </td>
          </tr>
        ))}
      </Table>
    </>
  );
});

function accountLabel(account: AccountFragment | null | undefined) {
  if (!account) return '—';
  return `${account.personId === null ? 'Klub' : (account.person?.name ?? '—')} (${account.currency})`;
}

function Transactions() {
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
  const [showFilters, setShowFilters] = React.useState(true);
  const [accounts] = useQuery({ query: TransactionAccountsDocument });
  const accountIds = filters.accountIds.length > 0 ? filters.accountIds : null;
  const period = { unit: filters.unit, date: filters.date ?? new Date() };
  const range = getPeriodRange(period.unit, period.date);
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: TransactionsDocument,
    variables: (offset) => ({
      first: TRANSACTIONS_PAGE_SIZE,
      offset,
      accountIds,
      condition: {
        accountIds,
        sources:
          filters.sources.length === defaultSources.length ? null : filters.sources,
        date: {
          since: range.since.toISOString(),
          until: range.until.toISOString(),
        },
        search,
      },
    }),
    getTotal: (data) => data.transactions?.totalCount,
    limit: TRANSACTIONS_PAGE_SIZE,
    pageKey: 'page',
    pause: searchPending,
  });
  const { total, page, limit } = pagination;
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
      <div className="flex flex-wrap items-center gap-2 py-2">
        <p
          role="status"
          className="whitespace-nowrap text-xs tabular-nums text-neutral-11"
        >
          {total === 0
            ? '0 / 0'
            : `${(page - 1) * limit + 1}–${Math.min(page * limit, total)} / ${total}`}
        </p>

        <button
          type="button"
          aria-expanded={showFilters}
          className={buttonCls({ display: 'compact', variant: 'facet', size: 'sm' })}
          onClick={() => setShowFilters((x) => !x)}
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
            onClick={() =>
              change({
                accountIds: [],
                sources: defaultSources,
                unit: 'schoolyear',
                date: null,
                search: '',
              })
            }
          >
            Zrušit filtry
          </button>
        )}
        <TextField
          name="search"
          type="search"
          aria-label="Vyhledat transakce"
          placeholder="Popis, VS nebo SS…"
          className="min-w-40 flex-1 sm:max-w-64 ml-auto"
          inputClassName="border-accent-6 bg-accent-2 py-1.5 text-sm text-neutral-12 placeholder:text-neutral-10 focus:border-accent-8 focus:ring-accent-8"
          value={filters.search}
          onChange={(event) => change({ search: event.currentTarget.value })}
        />
      </div>

      <div
        className={cn(
          'mb-3 w-full grid-cols-1 gap-x-3 gap-y-2 sm:grid-cols-[max-content_minmax(0,1fr)]',
          showFilters ? 'grid' : 'hidden',
        )}
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
          options={sourceOptions}
          onChange={(sources) => change({ sources })}
        />
        <FacetFilter
          inline
          label="Účty"
          value={filters.accountIds}
          maxVisibleOptions={5}
          moreLabel="Další"
          options={(accounts.data?.accountsList ?? [])
            .toSorted(
              (a, b) =>
                Number(a.personId !== null) - Number(b.personId !== null) ||
                compareText(accountLabel(a), accountLabel(b)),
            )
            .map((x) => ({ value: x.id, label: accountLabel(x) }))}
          onChange={(accountIds) => change({ accountIds })}
        />
        <PeriodPicker
          name="transactions-period"
          value={period}
          onChange={({ unit, date }) => change({ unit, date })}
        />
      </div>

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
              className="text-sm tabular font-normal"
              items={(accounts.data?.accountsList || []).flatMap((x) =>
                filters.accountIds.includes(x.id)
                  ? ([
                      [
                        accountLabel(x),
                        moneyFormatter.format({
                          amount: x.balance ?? '0',
                          currency: x.currency,
                        }),
                      ],
                    ] as const)
                  : [],
              )}
            />
          )
        }
        busy={fetching || searchPending}
        error={error ?? accounts.error}
      >
        {(data?.transactions?.nodes ?? []).flatMap((transaction) => {
          const description =
            transaction.description || describePosting(transaction.payment, 'neutral');

          return transaction.postingsList.map(({ id, amount, account }, i) => (
            <tr key={id} className="border-t border-neutral-4 align-top">
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
                        {description}
                      </Link>
                    ) : (
                      description
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
              <th scope="row" className="pl-0!">
                {accountLabel(account)}
              </th>
              <td className="whitespace-nowrap text-right">
                {account
                  ? moneyFormatter.format({ amount, currency: account.currency })
                  : amount}
              </td>
            </tr>
          ));
        })}
      </Table>
      <Pagination {...pagination} />
    </>
  );
}
